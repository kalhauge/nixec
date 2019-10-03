{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Nixec.Rule where

-- lens
import Control.Lens

-- base
import System.Exit
import Data.Foldable

-- mtl
import Control.Monad.State

-- text
import qualified Data.Text as Text

-- nixec
import Nixec.Data
import Nixec.Command

-- | The `Rule` is the basic item in `Nixec` this is what we are going to write
-- the whole program out of.
data Rule = Rule
  { _ruleRequires :: [ Requirement ]
  -- ^ The rule then have some requirements
  , _ruleCommands :: [ Command ]
  -- ^ The the rule has some commands to be executed
  , _ruleChecks :: [ Condition ]
  -- ^ And finally a rule has post-conditions. If these are not meet, the rule
  -- have failed to run.
  }

emptyRule :: Rule
emptyRule =
  Rule [] [] []

-- | The requirement are currently just depending on the output of other
-- rules to be put in some directories.
data Requirement
  = LinkTo FilePath Input
  | OnPath Package
  | Env Text.Text Input

infixr 4 ~>
-- | A convinient wrapper around `LinkTo`
(~>) :: HasInput i => FilePath -> i -> Requirement
(~>) fp = LinkTo fp . toInput

toFilePath :: Name -> FilePath
toFilePath = Text.unpack

-- | Condition
data Condition
  = FileExist FilePath
  -- ^ Checks that a file exist after running the program
  | ExitCodeWas ExitCode
  -- ^ Check that the exit-code is `ExitCode`.
  | Terminated
  -- ^ Check that the program terminated.

makeClassy ''Rule
makePrisms ''Requirement
makePrisms ''Condition

ruleInputs :: Rule -> [Input]
ruleInputs = toListOf
  $ ruleRequires.folded.(_LinkTo._2 <> _OnPath.to PackageInput <> _Env._2)

type RuleM = State Rule

safe :: RuleM ()
safe = do
  ruleChecks %= (++ [ Terminated, ExitCodeWas ExitSuccess ])

needs :: [ Requirement ] -> RuleM ()
needs regs = ruleRequires %= (++ regs)

cmd :: CommandArgument -> State Command () -> RuleM ()
cmd name statecmd =
  ruleCommands %= (++ [ Command name [] Nothing Nothing &~ statecmd ])

path :: [ Package ] -> RuleM ()
path pkgs =
  ruleRequires %= (++ [ OnPath p | p <- pkgs ])

joinCsv :: Foldable f => [ Text.Text ] -> f CommandArgument -> FilePath -> RuleM ()
joinCsv _ cmdargs fp = do
  cmd "cat" $ do
    args .= toList cmdargs
    stdout .= Just fp

exists :: FilePath -> RuleM ()
exists _ = return ()

copy :: Bool -> CommandArgument -> FilePath -> RuleM ()
copy recursive cm fp =
  cmd "cp" $ do
    args .= [ "-r" | recursive ] ++ [ cm , Output fp ]

link :: FilePath -> Input -> RuleM CommandArgument
link fp i = do
  needs [fp ~> i]
  return $ Input fp

links :: Traversable f => f (FilePath, Input) -> RuleM (f CommandArgument)
links = mapM (uncurry link)

asLinks :: Traversable f => f RuleName -> RuleM (f CommandArgument)
asLinks =
  links . fmap (\c -> (toFilePath (topRuleName c), RuleInput c))


  -- [text|
  --   { dependencies .. }:
  --   stdenv.mkDerivation {
  --     name = "<rulename>"
  --     unpackPhase = ''
  --       ln -s
  --     ''
  --     buildPhase = ''
  --       ./run.sh
  --     ''
  --     installPhase = ''
  --       mv . $out
  --     ''
  --   }|]
