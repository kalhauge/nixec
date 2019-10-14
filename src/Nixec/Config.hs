{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Nixec.Config where

-- base
import Prelude hiding (log)
import Data.Maybe
import System.IO.Error

-- -- text
-- import qualified Data.Text as Text

-- dirtree
import System.DirTree

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
import Nixec.Data
import Nixec.Rule
import qualified Nixec.Logger as L
import Nixec.Nix

data Config = Config
  { _configScope      :: !Scope
  , _configLogger     :: !L.Logger
  , _configDatabase   :: !FilePath
  , _configPathLookup :: !(Map.Map Input FilePath)
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
  => Input
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
  liftIO
    . writeFile (fp </> show (ruleNameToNix rn) <.> "nix")
    . show $ ruleToNix mkRule rn r

parseConfig :: Parser (IO Config)
parseConfig = do
  _configScope <- pure []

  xconfigDatabase <- optional . strOption $
    long "db"
    <> help "the path to a database."
    <> hidden
    <> metavar "DATABASE"

  _configLogger <- L.parseLogger

  _configMkRule <- pure "mkRule"

  xconfigTarget <- strArgument $
    metavar "DATABASE"
    <> help "The path to the database to create."

  return $ flip runReaderT _configLogger $ do

    _configTarget <- liftIO $ makeAbsolute xconfigTarget

    let yconfigDatabase = (fromMaybe _configTarget xconfigDatabase)

    _configDatabase <- liftIO (checkFileType yconfigDatabase) >>= \case
      Just (Directory _) ->
        liftIO $ makeAbsolute yconfigDatabase
      _ ->
        L.criticalFailure $ "Expected "
          <> L.displayString yconfigDatabase
          <> " to be a directory"

    let databaseFile = _configDatabase </> "database.csv"
    _configPathLookup <- readPathLookup databaseFile >>= \case
      Right lk -> return lk
      Left msg ->
        L.criticalFailure $ "Could not read "
        <> L.displayString databaseFile <> ": "
        <> L.displayString msg

    return $ Config {..}

  where
    readPathLookup db = runExceptT $ do
      bs <- withExceptT show . tryIOErrorT $ BL.readFile db
      (_, items) <- liftEither $ Csv.decodeByName bs
      return . Map.fromList . map (unPathLookup) . V.toList $ items

    tryIOErrorT = ExceptT . liftIO . tryIOError



-- instance HasNixConfig Config where
--   nixConfig = configNixConfig

instance L.HasLogger Config where
  logger = configLogger
