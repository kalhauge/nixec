{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Nixec.Rule where

-- lens
import Control.Lens

-- cassava
import qualified Data.Csv as Csv

-- vector
import qualified Data.Vector as V

-- filepath
import System.FilePath

-- base
import System.Exit
import Data.Foldable
import Data.Function
import Data.String
import Data.Maybe
import Data.Data
import Data.List.NonEmpty (NonEmpty(..), head, nonEmpty, tail)
import Data.List as List

-- mtl
import Control.Monad.State

-- text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyText

-- nixec
import Nixec.Command
import qualified Nixec.Logger as L

-- | A Name is just text
type Name = Text.Text

-- | A rule is a non-empty list of names
newtype RuleName = RuleName
  { unRuleName :: NonEmpty Name
  } deriving (Show, Eq, Ord, Data)

makeWrapped ''RuleName

-- | A Scope is just a list of names.
type Scope = [Name]

makeRuleName :: Name -> Scope -> RuleName
makeRuleName n s = RuleName $ n :| s

topRuleName :: RuleName -> Name
topRuleName (RuleName n) = Data.List.NonEmpty.head n

ruleNameToText :: RuleName -> Text.Text
ruleNameToText =
  LazyText.toStrict . LazyText.toLazyText . displayRuleName

ruleNameToString :: RuleName -> String
ruleNameToString =
  Text.unpack . ruleNameToText

ruleNameScope :: RuleName -> Scope
ruleNameScope = Data.List.NonEmpty.tail . unRuleName

ruleNameFromText :: Text.Text -> RuleName
ruleNameFromText =
  RuleName
  . fromJust . nonEmpty
  . reverse . Text.split (== '/')

ruleNameFromString :: String -> RuleName
ruleNameFromString =
  ruleNameFromText . Text.pack

-- | Check is a RuleName is in Scope
ruleNameInScope :: RuleName -> Scope -> Bool
ruleNameInScope (RuleName n) sp =
  sp `List.isSuffixOf` toList n
  || toList n `List.isSuffixOf` sp

removeScopePrefix :: Scope -> RuleName -> RuleName
removeScopePrefix scp rn@(RuleName (n :| s)) =
  case reverse scp `List.stripPrefix` reverse s of
    Just s' -> RuleName (n :| reverse s')
    Nothing -> rn


ruleNamePrefixLength :: RuleName -> RuleName -> Int
ruleNamePrefixLength =
  longestPrefix `on` reverse . toList . unRuleName
  where
    longestPrefix = curry $ \case
      (a:as, b:bs)
        | a == b -> 1 + (longestPrefix as bs)
      _ -> 0

displayRuleName :: RuleName -> LazyText.Builder
displayRuleName =
  foldr1 (\a b -> b <> "/" <> a) . fmap L.display . unRuleName

instance L.Display RuleName where
  display = displayRuleName

-- | We can use nix packages as inputs
newtype Package = Package (NonEmpty Text.Text)
  deriving (Show, Eq, Ord, Data)

superPackage :: Package -> Package
superPackage (Package p)=
  Package (Data.List.NonEmpty.head p :| [])

packageFromText :: Text.Text -> Package
packageFromText = Package
  . fromJust . nonEmpty
  . Text.split (=='.')

packageToText :: Package -> Text.Text
packageToText =
  LazyText.toStrict . LazyText.toLazyText . displayPackage

displayPackage :: Package -> LazyText.Builder
displayPackage (Package n) =
  foldl1 (\a b -> a <> "." <> b) $ fmap L.display n

instance IsString Package where
  fromString = packageFromText . Text.pack

instance L.Display Package where
  display = displayPackage

-- | We have different kinds of inputs
data Input
  = RuleInput RuleName
  | PackageInput Package
  | FileInput FilePath
  deriving (Show, Eq, Ord, Data)

makePrisms ''Input

-- | An input might be in a file
data InputFile
  = InputFile
  { _inputFileInput :: Input
  , _inputFileRelativePath :: FilePath
  } deriving (Show, Eq, Ord, Data)

makeLenses ''InputFile

pkg :: Package -> Input
pkg = PackageInput

class HasInputFile a where
  toInputFile :: a -> InputFile

instance HasInputFile InputFile where
  toInputFile = id

instance HasInputFile Input where
  toInputFile = flip InputFile ""

instance HasInputFile RuleName where
  toInputFile = toInputFile . RuleInput

instance HasInputFile Package where
  toInputFile = toInputFile . PackageInput

infixl 5 <./>
(<./>) :: HasInputFile i => i -> FilePath -> InputFile
a <./> fp =
  let InputFile b f = toInputFile a
  in InputFile b (f </> fp)

data Status
  = Success
  | Failure
  | Timeout
  deriving (Eq, Ord, Show)

data NixecStats = NixecStats
  {
  -- _statsRuleName :: RuleName
  -- , _statsStatus  :: Status
    _statsExitCode  :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''NixecStats

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
  = LinkTo FilePath InputFile
  | OnPath Package
  | Env Text.Text InputFile
  | CreateFile FilePath Bool Text.Text
  deriving (Show)

infixr 4 ~>
-- | A convinient wrapper around `LinkTo`
(~>) :: HasInputFile i => FilePath -> i -> Requirement
(~>) fp = LinkTo fp . toInputFile

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

ruleInputFiles :: Rule -> [InputFile]
ruleInputFiles = toListOf
  $ ruleRequires.folded.fold [_LinkTo._2, _OnPath.to toInputFile, _Env._2]

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

joinCsv ::
  Foldable f
  => [ Text.Text ]
  -> f CommandArgument -> FilePath -> RuleM ()
joinCsv _ cmdargs fp = do
  cmd "awk" $ do
    args .= "FNR==1 && NR!=1{next;}{print}":[c <.+> "/" <.+> fromString fp | c <- toList cmdargs ]
    stdout .= Just fp

exists :: FilePath -> RuleM ()
exists _ = return ()

copy :: Bool -> CommandArgument -> FilePath -> RuleM ()
copy recursive cm fp =
  cmd "cp" $ do
    args .= [ "-r" | recursive ] ++ [ cm , Output fp ]

link :: HasInputFile i => FilePath -> i -> RuleM CommandArgument
link fp i = do
  needs [fp ~> i]
  return $ Input fp

links :: Traversable f => f (FilePath, InputFile) -> RuleM (f CommandArgument)
links = mapM (uncurry link)

asLinks :: Traversable f => f RuleName -> RuleM (f CommandArgument)
asLinks =
  links . fmap (\c -> (toFilePath (topRuleName c), toInputFile c))

createFile :: FilePath -> Bool -> Text.Text -> RuleM CommandArgument
createFile fp b i = do
  needs [ CreateFile fp b i ]
  return $ Input fp

createScript :: FilePath -> Text.Text -> RuleM CommandArgument
createScript fp i = do
  createFile fp True i

-- ** Intances


instance Csv.FromField Status where
  parseField = \case
    "success" -> pure $ Success
    "failure" -> pure $ Failure
    "timeout" -> pure $ Timeout
    _ -> mzero

instance Csv.FromField RuleName where
  parseField a =
    ruleNameFromText <$> Csv.parseField a

instance Csv.ToField RuleName where
  toField = Csv.toField . ruleNameToText

instance Csv.ToField Package where
  toField = Csv.toField . packageToText

instance Csv.FromNamedRecord NixecStats where
  parseNamedRecord m = do
    -- _statsRuleName <- m Csv..: "rule"
    _statsExitCode <- m Csv..: "exitcode"
    pure $ NixecStats {..}


instance Csv.FromField Package where
  parseField = fmap packageFromText . Csv.parseField

instance Csv.FromNamedRecord InputFile where
  parseNamedRecord m = do
    itype <- m Csv..: "type"
    a <- case itype :: Text.Text of
      "rule"    -> RuleInput <$> m Csv..: "value"
      "package" -> PackageInput <$> m Csv..: "value"
      "file"    -> FileInput <$> m Csv..: "value"
      _ -> mzero
    file <- m Csv..: "file"
    return $ InputFile a file

instance Csv.DefaultOrdered InputFile where
  headerOrder _ = V.fromList [ "type", "value", "file" ]

instance Csv.ToNamedRecord InputFile where
  toNamedRecord (InputFile a fp) = case a of
    RuleInput r -> Csv.namedRecord
      [ "type" Csv..= ("rule" :: Text.Text)
      , "value" Csv..= r
      , "file" Csv..= fp
      ]
    PackageInput r -> Csv.namedRecord
      [ "type" Csv..= ("package" :: Text.Text)
      , "value" Csv..= r
      , "file" Csv..= fp
      ]
    FileInput r -> Csv.namedRecord
      [ "type" Csv..= ("file" :: Text.Text)
      , "value" Csv..= r
      , "file" Csv..= fp
      ]

newtype PathLookup = PathLookup { unPathLookup :: (InputFile, FilePath) }

instance L.Display InputFile where
  display (InputFile i fp) =
    if null fp then L.display i else "$(" <> L.display i <> ")/" <> L.displayString fp

instance L.Display Input where
  display = \case
    FileInput i -> "file:" <> L.displayString i
    PackageInput i -> "package:" <> L.display i
    RuleInput i -> "rule:" <> L.display i

instance L.Display PathLookup where
  display (PathLookup (inf, fp)) = L.display inf <> " -> " <> L.displayString fp

instance Csv.FromNamedRecord PathLookup where
  parseNamedRecord m = do
    i <- Csv.parseNamedRecord m
    o <- m Csv..: "output"
    return $ PathLookup (i, o)

instance Csv.DefaultOrdered PathLookup where
  headerOrder _ = V.fromList [ "type", "value", "file", "output"]

instance Csv.ToNamedRecord PathLookup where
  toNamedRecord (PathLookup (i, fp)) =
    Csv.toNamedRecord i <> Csv.namedRecord [ "output" Csv..= fp ]
