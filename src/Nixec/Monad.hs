{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Nixec.Monad where

-- base
import Prelude hiding (log)
import Data.Monoid

-- lens
import Control.Lens

-- directory
-- import System.Directory

-- mtl
import Control.Monad.Reader
import Control.Monad.Writer

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- Nixec
import Nixec.Data
import Nixec.Rule hiding (rule)


data Action
  = ListAction
  | ToNixAction RuleName

data Config = Config
  { _configScope  :: Scope
  , _configFolder :: FilePath
  , _configAction :: Action
  }

defaultConfig :: Config
defaultConfig =
  Config [] "_nixec" (ToNixAction $ ruleNameFromText "untar")

makeClassy ''Config

type NixecM = ReaderT Config (Writer (Endo [Rule]))

addRule :: Rule -> NixecM RuleName
addRule r = do
  tell (Endo (r:))
  return $ r^.ruleName

-- | The entry point of `Nixec`
defaultMain :: NixecM RuleName -> IO ()
defaultMain =
  mainWithConfig defaultConfig

mainWithConfig :: Config -> NixecM RuleName -> IO ()
mainWithConfig cfg mn = do
  let (_, flip appEndo [] -> rules) = runWriter (runReaderT mn cfg)
  case cfg^.configAction of
    ListAction -> do
      forM_ rules $ \r -> do
        print $ pretty (r ^. ruleName)
    ToNixAction n ->
      case findOf folded (view $ ruleName . to (==n)) rules of
        Just r ->
          print $ ruleToNix r
        Nothing ->
          error $ "No such rule: " ++ show (pretty n)

  -- createDirectoryIfMissing True (cfg^.configFolder)
  -- print (rulesToNix rules)

scope :: Name -> NixecM a -> NixecM a
scope name =
  local (configScope %~ (name:))

rule :: Name -> RuleM () -> NixecM RuleName
rule n rulem = do
  ns <- view configScope
  let name = makeRuleName n ns
  addRule $ makeRule name &~ rulem

-- | finish a scope
collect :: RuleM () -> NixecM RuleName
collect rulem = do
  name <- view configScope >>= \case
    n:ns -> return $ makeRuleName n ns
    [] -> return $ makeRuleName "all" []
  addRule $ makeRule name &~ rulem

onSuccess :: RuleName -> NixecM a -> NixecM (Maybe a)
onSuccess _ x =
  Just <$> x
