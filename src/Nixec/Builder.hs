{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Nixec.Builder where

-- base
import Prelude hiding (log)
import Data.Foldable
import System.IO.Error

-- directory
import System.Directory

-- filepath
import System.FilePath

-- vector
import qualified Data.Vector as V

-- cassava
import qualified Data.Csv as Csv

-- containers
import qualified Data.Map as Map
import qualified Data.Set as Set

-- mtl
import Control.Monad.Reader
import Control.Monad.Except

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- lens
import Control.Lens hiding ((<.>))

-- optparse-applicative
import Options.Applicative hiding (Success)

-- nixec
import Nixec.Monad
import Nixec.Rule
import qualified Nixec.Nix
import qualified Nixec.Logger as L

data Config = Config
  { _configScope      :: !Scope
  , _configLogger     :: !L.Logger
  , _configPathLookup :: !(Map.Map InputFile FilePath)
  , _configTarget     :: !FilePath
  , _configMkRule     :: !Package
  }

-- defaultConfig :: Config
-- defaultConfig = Config
--   []
--   (NixConfig "_nixec/nix" [ "overlay.nix" ] True "mkRule" ("nix-build", []))
--   Nothing
--   $ RunAction

makeClassy ''Config

-- | Update the scope of a config
inScope ::
  (MonadReader env m, HasConfig env)
  => Name
  -> m a
  -> m a
inScope name =
  local $ configScope %~ (name:)

-- | Create a new rule
newRuleName ::
  (HasConfig env, MonadReader env m)
  => Name
  -> m RuleName
newRuleName name = do
  makeRuleName name <$> view configScope

-- | Return a Maybe an absolute FilePath.
lookupDatabase ::
  (HasConfig env, MonadReader env m)
  => InputFile
  -> m (Maybe FilePath)
lookupDatabase i = do
  pl <- view configPathLookup
  return $ Map.lookup i pl

-- | Write Rule to the Target
writeRule ::
  (HasConfig env, MonadReader env m, MonadIO m)
  => FilePath
  -> RuleName
  -> Rule
  -> m ()
writeRule fp rn r = do
  mkRule <- view configMkRule
  Nixec.Nix.writeRule fp mkRule rn r

parseConfig :: Parser (IO Config)
parseConfig = do
  _configScope <- pure []

  xconfigDatabase <- many . strOption $
    long "db"
    <> help "the path to the database csv file."
    <> hidden
    <> metavar "DATABASE"

  _configLogger <- L.parseLogger

  _configMkRule <- pure "mkRule"

  xconfigTarget <- strArgument $
    metavar "NIXBASE"
    <> help "The path to the nixbase to create."

  return $ flip runReaderT _configLogger $ do

    _configTarget <- liftIO $ makeAbsolute xconfigTarget

    liftIO (createDirectoryIfMissing True _configTarget)

    _configPathLookup <- fmap fold . forM xconfigDatabase $ \db ->
      readPathLookup db >>= \case
        Right lk -> return lk
        Left msg -> do
          L.warning $ "Could not read "
            <> L.displayString db <> ": "
            <> L.displayString msg
          return mempty

    return $ Config {..}

  where
    readPathLookup db = runExceptT $ do
      bs <- withExceptT show . tryIOErrorT $ BL.readFile db
      (_, items) <- liftEither $ Csv.decodeByName bs
      return . Map.fromList . map (unPathLookup) . V.toList $ items

    tryIOErrorT = ExceptT . liftIO . tryIOError

instance L.HasLogger Config where
  logger = configLogger

-- | The entry point of `Nixec`
defaultMain ::
  Nixec Rule
  -> IO ()
defaultMain nscript = do
  iocfg <- execParser $
    info (parseConfig <**> helper) (header "nixec-builder")
  cfg <- iocfg
  mainWithConfig cfg nscript

mainWithConfig :: Config -> Nixec Rule -> IO ()
mainWithConfig cfg nm = flip runReaderT cfg $ do
  L.info "Running Nixec"
  trg <- view configTarget

  pths <- view configPathLookup
  L.info $ "Running paths" <> L.displayShow pths

  liftIO $ createDirectoryIfMissing True (trg </> "rules")
  L.phase "database" $ buildDatabase (trg </> "rules") (void . addRule "all" =<< nm) >>= \case
    Right () -> L.info "Success"
    Left missing -> do
      L.info $ "Missing " <> L.displayShow (Set.size missing) <> " inputs."
      liftIO . BL.writeFile (trg </> "missing.csv") $
        Csv.encodeDefaultOrderedByName (toList missing)

      liftIO . writeFile (trg </> "database.nix")
        $ show (Nixec.Nix.databaseExpr missing)

  db <- view configPathLookup
  liftIO . BL.writeFile (trg </> "database.csv")
    $ Csv.encodeDefaultOrderedByName (map PathLookup $ Map.toList db)

buildDatabase ::
  forall env m a.
  (HasConfig env, L.HasLogger env, MonadReader env m, MonadIO m)
  => FilePath
  -> Nixec a
  -> m (Either (Set.Set InputFile) a)
buildDatabase trg = runExceptT . runNixec run where
  run :: NixecF (ExceptT (Set.Set InputFile) m b) -> ExceptT (Set.Set InputFile) m b
  run = \case
    AddRule name r x -> do
      rn <- newRuleName name
      L.debug $ "Adding rule " <> L.displayShow rn
      writeRule trg rn r
      x rn

    Scope name nm x -> do
      L.debug $ "Entering scope: " <> L.displayShow name
      r <- inScope name $ runNixec run nm
      L.debug $ "Exit scope: " <> L.displayShow name
      run (AddRule name r x)

    InspectInput i io na -> do
      L.debug $ "Inspecting: " <> L.displayShow i
      lookupDatabase i >>= \case
        Just output -> do
          L.debug $ "Lookup succeded"
          na =<< liftIO (io output)
        Nothing -> do
          L.debug $ "Could not be found"
          throwError (Set.singleton i)

    Seperate n1 n2 n -> do
      x <- ExceptT $ liftA2 (,)
          (buildDatabase trg n1) (buildDatabase trg n2) <&> \case
        (Right a, Right b) -> Right (a, b)
        (Left s1, Left s2) -> Left (s1 <> s2)
        (Right _, Left s2) -> Left s2
        (Left s1, Right _) -> Left s1
      n x
