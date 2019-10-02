{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Nixec.Monad where

-- base
import Prelude hiding (log)
import Data.Monoid
import Control.Monad.Fail
import qualified Data.List as List

-- cassava
import qualified Data.Csv as Csv

-- typed-process
import System.Process.Typed

-- free
import Control.Monad.Free
import Control.Monad.Free.TH

-- lens
import Control.Lens hiding ((<.>))

-- filepath
import System.FilePath

-- directory
import System.Directory

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- mtl
import Control.Monad.Reader
import Control.Monad.Writer

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- Nixec
import Nixec.Data
import Nixec.Rule hiding (rule)
import Nixec.Nix

type Nixec = Free NixecF

data NixecF x
  = AddRule Name Rule (RuleName -> x)
  | Scope Name (Nixec Rule) (RuleName -> x)
  | forall a. OnSuccess RuleName (Nixec a) (a -> x) x

deriving instance (Functor NixecF)

makeFree_ ''NixecF

addRule :: MonadFree NixecF m => Name -> Rule -> m RuleName
scope   :: MonadFree NixecF m => Name -> Nixec Rule -> m RuleName
onSuccess :: MonadFree NixecF m => RuleName -> Nixec a -> m (Maybe a)

data Action
  = ListAction
  | ToNixAction RuleName
  | ExecAction RuleName

data Config = Config
  { _configScope  :: Scope
  , _configFolder :: FilePath
  , _configOverlays :: [ FilePath ]
  , _configAction :: Action
  }

defaultConfig :: Config
defaultConfig =
  Config
  [] "example/_nixec"
  [ "example/overlay.nix" ]
  $ ExecAction (ruleNameFromText "urlfc5806b04b_wlu_mstr_leveldb_java:cfr:run")

makeClassy ''Config

newRuleName :: (HasConfig env, MonadReader env m) => Name -> m RuleName
newRuleName name = do
  makeRuleName name <$> view configScope

runListAction :: (MonadReader env m, HasConfig env) => Nixec Rule -> m [RuleName]
runListAction =
  fmap (flip appEndo [makeRuleName "all" []]) . execWriterT . iterM run
  where
    run ::
      (HasConfig env, MonadReader env m', MonadWriter (Endo [RuleName]) m')
      => NixecF (m' a)
      -> m' a
    run = \case
      AddRule name _ x -> do
        rn <- newRuleName name
        tell $ Endo (rn:)
        x rn
      Scope name nm x -> do
        n <- view configScope
        _ <- local (configScope .~ name:n) $ iterM run nm
        rn <- newRuleName name
        tell $ Endo (rn:)
        x rn
      OnSuccess rn nm na _ -> do
        _ <- iterM run nm
        na (error $ "The success of '" ++ show (pretty rn) ++ "'cannot effect the number of rules.")

runExecAction ::
  forall env m.
  (MonadReader env m, HasConfig env, MonadIO m, MonadFail m)
  => RuleName
  -> Nixec Rule
  -> m ()
runExecAction target nscript = do
  nixDir <- (</> "nix") <$> view configFolder
  liftIO $ do
    createDirectoryIfMissing True nixDir

  r <- iterM run nscript

  if target == ruleNameFromText "all"
    then writeNixRule target r
    else return ()

  nixBuild "result" target

  where
    nixFile rn =
      view configFolder <&> \cnf ->
        cnf </> "nix" </> show (ruleNameToNix rn) <.> "nix"

    nixBuild output rn = do
      file <- nixFile rn
      fps <- view configOverlays
      overlays <- fmap concat . forM fps $ \fp -> do
        b <- liftIO $ doesFileExist fp
        return [ "(import ./" ++ fp ++ ")"| b ]

      runProcess_
        . setDelegateCtlc True
        $ proc "nix-build"
          [ "-E",
            "(import <nixpkgs> { overlays = [ "
            ++ List.intercalate " " overlays
            ++ " ]; }).callPackage " ++ file ++ " {}"
          , "-o", output
          ]

    writeNixRule rn r = do
      file <- nixFile rn
      liftIO . writeFile file $ show (ruleToNix rn r)

    run :: NixecF (m a) -> m a
    run = \case
      AddRule name r mn -> do
        rn <- newRuleName name
        writeNixRule rn r
        mn rn

      Scope name nm x -> do
        n <- view configScope
        r <- local (configScope .~ name:n) $ iterM run nm
        rn <- newRuleName name
        writeNixRule rn r
        x rn

      OnSuccess rn nm na n -> do
        output <- dropExtension <$> nixFile rn
        nixBuild output rn
        Right (_, vn) <-
          liftIO
          . fmap Csv.decodeByName
          . BL.readFile
          $ output </> "nixec.csv"
        case findOf folded (view $ statsRuleName.to (== rn)) vn of
          Just stat
            | stat ^.statsStatus == Success -> do
              a <- iterM run nm
              na a
          _ -> n

-- | The entry point of `Nixec`
defaultMain :: Nixec Rule -> IO ()
defaultMain =
  mainWithConfig defaultConfig

mainWithConfig :: Config -> Nixec Rule -> IO ()
mainWithConfig cfg nm = do
  case cfg^.configAction of
    ListAction -> do
      mapM_ (print . pretty) $ runReader (runListAction nm) cfg
    ExecAction name ->
      runReaderT (runExecAction name nm) cfg
    _ -> undefined

    -- ToNixAction n ->
    --   case findOf folded (view $ ruleName . to (==n)) rules of
    --     Just r ->
    --       print $ ruleToNix r
    --     Nothing ->
    --       error $ "No such rule: " ++ show (pretty n)

  -- createDirectoryIfMissing True (cfg^.configFolder)
  -- print (rulesToNix rules)

-- scope :: Name -> NixecM a -> NixecM a
-- scope name =
  -- local (configScope %~ (name:))


rule :: Name -> RuleM () -> Nixec RuleName
rule n rulem = do
  addRule n $ emptyRule &~ rulem

-- | finish a scope
collect :: RuleM () -> Nixec Rule
collect rulem = do
  return $ emptyRule &~ rulem

-- onSuccess :: RuleName -> NixecM a -> NixecM (Maybe a)
-- onSuccess _ x =
--   Just <$> x
