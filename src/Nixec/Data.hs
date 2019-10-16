{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Nixec.Data where

-- lens
import Control.Lens

-- cassava
import qualified Data.Csv as Csv

-- filepath
import System.FilePath

-- vector
import qualified Data.Vector as V

-- base
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.Function
-- import System.Exit
import Data.Data
import Data.String
import qualified Data.List as List

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- text
import qualified Data.Text as Text
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText

type Name = Text.Text

newtype RuleName = RuleName
  { unRuleName :: NonEmpty.NonEmpty Name
  } deriving (Show, Eq, Ord, Data)

type Scope = [Name]

makeRuleName :: Name -> Scope -> RuleName
makeRuleName n s = RuleName $ n NonEmpty.:| s

topRuleName :: RuleName -> Name
topRuleName (RuleName n) = NonEmpty.head n

ruleNameToText :: RuleName -> Text.Text
ruleNameToText =
  LazyText.toStrict . toLazyText . displayRuleName

ruleNameToString :: RuleName -> String
ruleNameToString =
  Text.unpack . ruleNameToText

ruleNameFromText :: Text.Text -> RuleName
ruleNameFromText =
  RuleName
  . fromJust . NonEmpty.nonEmpty
  . reverse . Text.split (== '-')

ruleNameFromString :: String -> RuleName
ruleNameFromString =
  RuleName
  . fromJust . NonEmpty.nonEmpty
  . reverse . Text.split (== '-')
  . Text.pack

ruleNameInScope :: RuleName -> Scope -> Bool
ruleNameInScope (RuleName n) sp =
  sp `List.isSuffixOf` (NonEmpty.toList n)
  || (NonEmpty.toList n) `List.isSuffixOf` sp

ruleNamePrefixLength :: RuleName -> RuleName -> Int
ruleNamePrefixLength =
  longestPrefix `on` reverse . NonEmpty.toList . unRuleName
  where
    longestPrefix = curry $ \case
      (a:as, b:bs)
        | a == b -> 1 + (longestPrefix as bs)
      _ -> 0

displayRuleName :: RuleName -> Builder
displayRuleName =
  foldr1 (\a b -> b <> "-" <> a) . fmap fromText . unRuleName

instance Pretty RuleName where
  pretty = pretty . toLazyText . displayRuleName

-- | We can use nix packages as inputs
newtype Package = Package (NonEmpty.NonEmpty Text.Text)
  deriving (Show, Eq, Ord, Data)

superPackage :: Package -> Package
superPackage (Package p)=
  Package (NonEmpty.head p NonEmpty.:| [])

packageFromText :: Text.Text -> Package
packageFromText = Package
  . fromJust . NonEmpty.nonEmpty
  . Text.split (=='.')

packageToText :: Package -> Text.Text
packageToText (Package n) =
  Text.intercalate "." (NonEmpty.toList n)

instance IsString Package where
  fromString = packageFromText . Text.pack

instance Pretty Package where
  pretty = pretty . packageToText

-- | We have different kinds of inputs
data Input
  = RuleInput RuleName
  | PackageInput Package
  | FileInput FilePath
  | InInput Input FilePath
  deriving (Show, Eq, Ord, Data)


instance Pretty Input where
  pretty = \case
    RuleInput rn ->
      "rule:" <+> pretty rn
    PackageInput p ->
      "package:" <+> pretty p
    FileInput f ->
      "file:" <+> pretty f
    InInput i fp ->
      "file " <+> pretty fp <+> "in" <+> pretty i

makePrisms ''Input

pkg :: Package -> Input
pkg = PackageInput

class HasInput a where
  toInput :: a -> Input

instance HasInput Input where
  toInput = id

instance HasInput RuleName where
  toInput = RuleInput

-- | An optional thing might not exist in the real world
newtype Optional a = Optional { optionalContent :: a }

instance HasInput (Optional RuleName) where
  toInput = RuleInput . optionalContent

infixl 5 <./>
(<./>) :: HasInput i => i -> FilePath -> Input
(<./>) = InInput . toInput

log :: MonadIO m => Builder -> m ()
log = liftIO . LazyText.putStrLn . toLazyText

data Status
  = Success
  | Failure
  | Timeout
  deriving (Eq, Ord, Show)

data NixecStats = NixecStats
  { _statsRuleName :: RuleName
  -- , _statsStatus  :: Status
  , _statsExitCode  :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''NixecStats

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
    _statsRuleName <- m Csv..: "rule"
    _statsExitCode <- m Csv..: "exitcode"
    pure $ NixecStats {..}


instance Csv.FromField Package where
  parseField = fmap packageFromText . Csv.parseField

instance Csv.FromNamedRecord Input where
  parseNamedRecord m = do
    itype <- m Csv..: "type"
    a <- case itype :: Text.Text of
      "rule"    -> RuleInput <$> m Csv..: "value"
      "package" -> PackageInput <$> m Csv..: "value"
      "file"    -> FileInput <$> m Csv..: "value"
      _ -> mzero
    file <- m Csv..: "file"
    return $ maybe a (InInput a) file

instance Csv.DefaultOrdered Input where
  headerOrder _ = V.fromList [ "type", "value", "file" ]

instance Csv.ToNamedRecord Input where
  toNamedRecord = go Nothing where
    go a = \case
      RuleInput r -> Csv.namedRecord
        [ "type" Csv..= ("rule" :: Text.Text)
        , "value" Csv..= r
        , "file" Csv..= a
        ]
      PackageInput r -> Csv.namedRecord
        [ "type" Csv..= ("package" :: Text.Text)
        , "value" Csv..= r
        , "file" Csv..= a
        ]
      FileInput r -> Csv.namedRecord
        [ "type" Csv..= ("file" :: Text.Text)
        , "value" Csv..= r
        , "file" Csv..= a
        ]
      InInput r f ->
        go (Just $ maybe f (f </>) a) r

newtype PathLookup = PathLookup { unPathLookup :: (Input, FilePath) }

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
