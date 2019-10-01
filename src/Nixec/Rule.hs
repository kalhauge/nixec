{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Nixec.Rule where

-- lens
import Control.Lens

-- base
import System.Exit
import Data.Maybe

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- filepath
import System.FilePath

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
  { _ruleName :: RuleName
  -- ^ Every rule needs a name
  , _ruleRequires :: [ Requirement ]
  -- ^ The rule then have some requirements
  , _ruleCommands :: [ Command ]
  -- ^ The the rule has some commands to be executed
  , _ruleChecks :: [ Condition ]
  -- ^ And finally a rule has post-conditions. If these are not meet, the rule
  -- have failed to run.
  }

-- | Create a new `Rule`
makeRule :: RuleName -> Rule
makeRule n =
  Rule n [] [] []

-- | The requirement are currently just depending on the output of other
-- rules to be put in some directories.
data Requirement
  = LinkTo FilePath Input
  | OnPath Package

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

type RuleM = State Rule

safe :: RuleM ()
safe = do
  ruleChecks %= (++ [ Terminated, ExitCodeWas ExitSuccess ])

needs :: [ Requirement ] -> RuleM ()
needs regs = ruleRequires %= (++ regs)

cmd :: Text.Text -> State Command () -> RuleM ()
cmd name statecmd =
  ruleCommands %= (++ [ Command name [] Nothing Nothing &~ statecmd ])

path :: [ Package ] -> RuleM ()
path pkgs =
  ruleRequires %= (++ [ OnPath p | p <- pkgs ])

joinCsv :: [ Text.Text ] -> [ RuleName ] -> FilePath -> RuleM ()
joinCsv _ rules fp = do
  needs [ toFilePath (topRuleName c) ~> c | c <- rules ]
  cmd "cat" $ do
    commandArgs .= [ Input (toFilePath (topRuleName r) </> fp) | r <- rules ]
    commandStdout .= Just fp

exists :: FilePath -> RuleM ()
exists _ = return ()

copy :: Bool -> CommandArgument -> FilePath -> RuleM ()
copy recursive cm fp =
  cmd "cp" $ do
    commandArgs .= [ "-r" | recursive ] ++ [ cm , Output fp ]


ruleNameToNix :: RuleName -> Doc m
ruleNameToNix =
  foldr1 (\a b -> b <> "-" <> a) . fmap pretty . unRuleName


renderCommands :: [Command] -> [Doc m]
renderCommands commands =
  [ "WORKDIR=''${1:-\"$(pwd)\"}"
  , "INPUTDIR=''${1:-$WORKDIR}"
  ] ++
  [ hsep $ concat
    [ [ pretty (c^.commandProgram) ]
    , [ commandArgToShell ca | ca <- c^. commandArgs]
    , [ ">>" <> (dquotes . pretty $ "$WORKDIR" </> fp)
      | fp <- maybeToList (c ^. commandStdout)]
    , if c ^. commandStderr == c ^. commandStdout && c ^. commandStderr /= Nothing
      then [ "2>&1" ]
      else [ "2>>" <> (dquotes . pretty $ "$WORKDIR" </> fp)
           | fp <- maybeToList (c ^. commandStderr)]
    ]
  | c <- commands
  ]
  where
    commandArgToShell = \case
      Input fp -> dquotes . pretty $ "$INPUTDIR" </> fp
      Output fp -> dquotes . pretty $ "$WORKDIR" </> fp
      Global i -> inputToShell i
      RegularArg i
        | Text.any (\c -> c == ';' || c == ' ') i -> dquotes (pretty i)
        | otherwise -> pretty i
      ConcatArg t args ->
        concatWith (\a b -> a <> pretty t <> b) . map commandArgToShell $ args

nixToShell :: Doc m -> Doc m
nixToShell = enclose "${" "}"

inputToShell :: Input -> Doc m
inputToShell = \case
  RuleInput r -> nixToShell (ruleNameToNix r)
  PackageInput r -> nixToShell (pretty r)
  FileInput r -> nixToShell ("./" <> pretty r)
  InInput r fp -> inputToShell r <> "/" <> pretty fp

rulesToNix :: [Rule] -> Doc m
rulesToNix rules =
  "{ nixpkgs ? import <nixpkgs> {} }: with nixpkgs;" <+>
  ( attrset
    . map (\r -> (ruleNameToNix (r^.ruleName), ruleToNix r))
    $ rules
  )

attrset :: [(Doc m, Doc m)] -> Doc m
attrset =
  braces
  . enclose line line
  . indent 2
  . vsep
  . map (\(n, m) -> n <+> "=" <+> m <> ";")

letlist :: [(Doc m, Doc m)] -> Doc m -> Doc m
letlist lst inexp =
  "let"
  <+> ( enclose softline softline
       . indent 2
       . vsep
       . map (\(n, m) -> n <+> "=" <+> m <> ";")
       $ lst
     )
  <+> "in"
  <+> inexp

ruleToNix :: Rule -> Doc m
ruleToNix r =
  ("stdenv.mkDerivation" <+>) $
  attrset
  [ ( "name"
    , dquotes (pretty (r ^. ruleName))
    )
  , ( "buildInputs"
    , encloseSep "[" "]" " "
      [ pretty p
      | OnPath p <- r ^. ruleRequires
      ]
    )
  , ( "command"
    , "builtins.toFile" <+> dquotes "run.sh" <+> script (renderCommands (r ^. ruleCommands))
    )
  , ( "unpackPhase"
    , ( script . concat $
        [ [ "ln -s" <+> inputToShell i <+> squotes (pretty fp)
          | LinkTo fp i <- r ^. ruleRequires
          ]
        , [ "ln -s $command run.sh"
          , "chmod +x run.sh"
          ]
        ]
      )
    )
  , ( "buildPhase"
    , script [ "sh run.sh 2>&1 >>output.log"]
    )
  , ( "installPhase"
    , script [ "mv . $out" ]
    )
  ]

  where
    script a = line <>
      indent 2 (
        enclose ("''" <> softline) (softline <> "''")
        . align
        $ vsep a
        )

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
