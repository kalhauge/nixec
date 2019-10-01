{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Nixec.Data where

-- base
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.IO.Class
import Data.Maybe

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- text
import qualified Data.Text as Text
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as LazyText

type Name = Text.Text

newtype RuleName = RuleName
  { unRuleName :: NonEmpty.NonEmpty Name
  } deriving (Show, Eq, Ord)

type Scope = [Name]

makeRuleName :: Name -> Scope -> RuleName
makeRuleName n s = RuleName $ n NonEmpty.:| s

topRuleName :: RuleName -> Name
topRuleName (RuleName n) = NonEmpty.head n

ruleNameFromText :: Text.Text -> RuleName
ruleNameFromText = RuleName . fromJust . NonEmpty.nonEmpty . Text.split (== ':')

displayRuleName :: RuleName -> Builder
displayRuleName =
  foldr1 (\a b -> b <> ":" <> a) . fmap fromText . unRuleName

instance Pretty RuleName where
  pretty = pretty . toLazyText . displayRuleName

-- | We can use nix packages as inputs
type Package = Text.Text

-- | We have different kinds of inputs
data Input
  = RuleInput RuleName
  | PackageInput Package
  | FileInput FilePath
  | InInput Input FilePath

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
