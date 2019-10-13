{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nixec.App where

-- base
import Prelude hiding (log)
import Data.Foldable
import Data.Maybe

-- directory
import System.Directory

-- dirtree
import System.DirTree

-- containers
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

-- lens
import Control.Lens hiding ((<.>))

-- optparse-applicative
import Options.Applicative hiding (Success)

-- mtl
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Monad.State
import Control.Monad.Except

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- Nixec
import Nixec.Data
import Nixec.Config
import qualified Nixec.Logger as L
import Nixec.Rule hiding (rule)
import Nixec.Nix
import Nixec.Monad

-- | The entry point of `Nixec`
defaultMain ::
  Nixec Rule
  -> IO ()
defaultMain nscript = do
  cfg <- execParser $
    info (parseConfig <**> helper) (header "nixec-builder")
  mainWithConfig cfg nscript


mainWithConfig :: Config -> Nixec Rule -> IO ()
mainWithConfig cfg nm = flip runReaderT cfg $ do
  trg <- fromMaybe (makeRuleName "all" []) <$> view configTarget

  nixFileExist <- liftIO . doesFileExist =<< nixFile trg

  doCheck <- view configCheck
  when (doCheck || not nixFileExist) $ do
    liftIO $ putStrLn "Build Database:"
    continuouslyBuildDatabase

  view configDryRun >>= \case
    True -> do
      script <- nixRuleScript trg
      liftIO $ do
        putStrLn "Would run following script:"
        print script
    False -> do
      liftIO $ putStrLn "Build Target:"
      nixBuildInput (RuleInput trg) >>= \case
        Just output -> liftIO $ do
          putStrLn "Success"
          createDirectoryLink output "result"
        Nothing ->
          liftIO $ putStrLn "Failure"

  where
    continuouslyBuildDatabase = do
      (fst <$> buildDatabase (addRule "all" =<< nm)) >>= \case
        Right _ ->
          liftIO $ putStrLn "No Tasks..."

        Left s -> do
          liftIO $ do
            putStrLn "Tasks:"
            forM_ (Set.toAscList s) $ \rn ->
              print ("+ " <> pretty rn)

          doDryRun <- view configDryRun
          unless doDryRun $ do
            liftIO $ putStrLn "Computing tasks:"
            nixBuildAll s >>= \case
              Just _ ->
                continuouslyBuildDatabase
              Nothing ->
                liftIO $ putStrLn "Failed building tasks"

inScope :: (MonadReader env m, HasConfig env) => Name -> m a -> m a
inScope name =
  local $ configScope %~ (name:)

newRuleName ::
  (HasConfig env, MonadReader env m)
  => Name
  -> m RuleName
newRuleName name = do
  makeRuleName name <$> view configScope

type MissingT m a =
  ExceptT (Set.Set Input) (WriterT (Map.Map RuleName Rule) m) a

buildDatabase ::
  forall env m a.
  (HasConfig env, HasNix env m)
  => Nixec a
  -> m (Either (Set.Set Input) a, Map.Map RuleName Rule)
buildDatabase nscript
  = runWriterT (runExceptT . runNixec run $ nscript) where
  run :: NixecF (MissingT m b) -> MissingT m b
  run = \case
    AddRule name r x -> do
      rn <- newRuleName name
      nixFileExist <- liftIO . doesFileExist =<< nixFile rn
      doCheck <- view configCheck
      unless (nixFileExist || not doCheck) $
        writeNixRule rn r
      tell (Map.singleton rn r)
      view configTarget >>= \case
        Just target
          | rn == target ->
              throwError (Set.singleton (RuleInput rn))
        _ -> x rn

    Scope name nm x -> do
      r <- inScope name $ runNixec run nm
      run (AddRule name r x)

    Inspect i io na -> do
      nixCheckBuildInput i >>= \case
        Just output ->
          na =<< liftIO (io output)
        Nothing ->
          throwError (Set.singleton i)

    Seperate n1 n2 n -> do
      trg <- view configTarget
      (a, b) <- ExceptT . WriterT $ do
        (e1, rls1) <- buildDatabase n1
        (e2, rls2) <- buildDatabase n2
        case (e1, e2) of
          -- Both computed
          (Right a, Right b) ->
            return (Right (a, b), rls1 <> rls2)

          -- Both failed, choose the one with the longest prefix and
          -- report that back
          (Left s1, Left s2) -> do
            case trg of
              Just target -> do
                let prl1 = maxPrefixLength s1
                    prl2 = maxPrefixLength s2
                return $ case compare prl1 prl2 of
                  LT -> (Left s2, rls2)
                  GT -> (Left s1, rls1)
                  EQ -> (Left (s1 <> s2), rls1 <> rls2)
                where
                  maxInputLength = \case
                    RuleInput f -> ruleNamePrefixLength target f
                    _ -> 0
                  maxPrefixLength =
                    maximum . map maxInputLength . Set.toList
              Nothing ->
                return (Left (s1 <> s2), rls1 <> rls2)

          -- One failed, only include data to produce the fail
          (Left s1, _) ->
            return (Left s1, rls1)

          -- One failed, only include data to produce the fail
          (_, Left s2) ->
            return (Left s2, rls2)

      n (a, b)


data AppCommand
  = Run

data AppConfig = AppConfig
  { _appLogger     :: L.Logger
  , _appNixecfile  :: FilePath
  }

parseAppConfig :: Parser (IO AppConfig)
parseAppConfig = do
  _appLogger <-
    L.parseLogger

  ioAppNixecfile <- strOption $
    short 'f'
    <> long "file"
    <> help "the path to the Nixecfile.hs."
    <> value "./Nixecfile.hs"
    <> showDefault

  pure $ do

    _appNixecfile <- checkFileType ioAppNixecfile >>= \case
      Just (File _) ->
        makeAbsolute ioAppNixecfile
      _ ->
        flip runReaderT _appLogger . L.criticalFailure $ "Expected "
          <> L.displayString ioAppNixecfile
          <> "to be a file."

    return $ AppConfig {..}

makeClassy ''AppConfig

instance L.HasLogger AppConfig where logger = appLogger

app :: IO ()
app = do
  appCfg <- join . execParser $
    info (parseAppConfig <**> helper) (header "nixec")

  runReaderT appRun appCfg

appRun :: ReaderT AppConfig IO ()
appRun = do
  nixecfile <- view appNixecfile
  L.debug $ "Started Nixec."
  L.debug $ "Found Nixecfile: " <> L.displayString nixecfile
