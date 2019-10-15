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
-- import Data.String
import Data.Maybe
import Data.List.NonEmpty (NonEmpty (..))
import System.IO.Error

-- directory
import System.Directory

-- filepath
import System.FilePath

-- dirtree
import System.DirTree

-- -- data-fix
-- import Data.Fix

-- vector
import qualified Data.Vector as V

-- containers
import qualified Data.Set as Set
import qualified Data.Map as Map

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- cassava
import qualified Data.Csv as Csv

-- lens
import Control.Lens hiding ((<.>))

-- optparse-applicative
import Options.Applicative hiding (Success)

-- prettyprinter
-- import Data.Text.Prettyprint.Doc

-- mtl
import Control.Monad.Reader
import Control.Monad.Except

-- Nixec
import Nixec.Data
import Nixec.Nix
import Nixec.Config
import qualified Nixec.Logger as L
import Nixec.Rule hiding (rule)
import Nixec.Monad

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

  liftIO $ createDirectoryIfMissing True (trg </> "rules")
  buildDatabase (trg </> "rules") (void . addRule "all" =<< nm) >>= \case
    Right () -> L.info "Success"
    Left msg -> do
      L.info "Missing computations"
      liftIO . BL.writeFile (trg </> "missing.csv") $
        Csv.encodeDefaultOrderedByName (toList msg)

      liftIO . writeFile (trg </> "database.nix")
        $ show (nixMissing msg)

  db <- view configPathLookup
  liftIO . BL.writeFile (trg </> "database.csv")
    $ Csv.encodeDefaultOrderedByName (map PathLookup $ Map.toList db)

buildDatabase ::
  forall env m a.
  (HasConfig env, L.HasLogger env, MonadReader env m, MonadIO m)
  => FilePath
  -> Nixec a
  -> m (Either (Set.Set Input) a)
buildDatabase trg = runExceptT . runNixec run where
  run :: NixecF (ExceptT (Set.Set Input) m b) -> ExceptT (Set.Set Input) m b
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

    Inspect i io na -> do
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


data AppCommand
  = ListRules
  deriving (Show, Eq)

parseAppCommand :: Parser AppCommand
parseAppCommand =
  hsubparser $
  command "list"
   (info (pure ListRules)
    (progDesc "list the possible targets")
   )

data AppConfig = AppConfig
  { _appLogger         :: !L.Logger
  , _appNixecfile      :: !FilePath
  , _appNixecFolder    :: !FilePath
  , _appNixecDatabase  :: !FilePath
  , _appNixConfig      :: !NixConfig
  } deriving (Show, Eq)

parseAppConfig :: Parser (IO AppConfig)
parseAppConfig = do
  _appLogger <-
    L.parseLogger


  ioAppNixecfile <- strOption $
    short 'f'
    <> long "file"
    <> help "the path to the Nixecfile.hs."
    <> value "./Nixecfile.hs"
    <> hidden
    <> metavar "NIXECFILE"
    <> showDefault

  mappNixecFolder <- optional . strOption $
    long "nixec-folder"
    <> help "the path to the Nixec database. Normally NIXECFILE/../_nixec"
    <> hidden
    <> metavar "NIXECFOLDER"

  pure $ do

    _appNixecfile <- checkFileType ioAppNixecfile >>= \case
      Just (File _) ->
        makeAbsolute ioAppNixecfile
      _ ->
        flip runReaderT _appLogger . L.criticalFailure $ "Expected "
          <> L.displayString ioAppNixecfile
          <> " to be a file."

    let
      _appNixecFolder = fromMaybe (takeDirectory _appNixecfile </> "_nixec") mappNixecFolder
      _appNixecDatabase = _appNixecFolder </> "database"

      _appNixConfig =
        let
          _nixOverlays = [ _appNixecFolder </> "overlay.nix" ]
          _nixVerbose = True
          _nixBuildCommand = ("nix-build", [])
        in NixConfig {..}


    return $ AppConfig {..}

makeClassy ''AppConfig

instance L.HasLogger AppConfig where logger = appLogger
instance HasNixConfig AppConfig where nixConfig = appNixConfig

type App = ReaderT AppConfig IO

app :: IO ()
app = do
  (ioCfg, appCmd) <- execParser $
    info (((,) <$> parseAppConfig <*> parseAppCommand)
          <**> helper) (header "nixec")

  runReaderT (runapp appCmd) =<< ioCfg

runapp :: AppCommand -> ReaderT AppConfig IO ()
runapp appCmd = do
  nixecfile <- view appNixecfile
  cfg <- view appConfig
  L.info "Started Nixec."
  L.debug $ "Config: " <> L.displayShow cfg
  L.info  $ "Found Nixecfile: " <> L.displayString nixecfile

  calculateDatabase
 
  db <- readDatabase >>= \case
    Right db -> do
      L.info "Database succesfully read."
      return $ db
    Left missing -> do
      L.info $ "Could not read the database: "
        <> if Set.null missing
        then "no database."
        else "missing " <> L.displayShow (Set.size missing) <> " dependencies."

      liftIO $ print (nixMissing missing)

      calculateDatabase
      L.criticalFailure "Could not create database."

  L.info "done."
  case appCmd of
    ListRules -> do
      forM_ db $ \(rn, _) -> liftIO $ do
        putStrLn $ show rn

  where
    calculateDatabase :: App ()
    calculateDatabase = do
      L.info "Calculating the database"

      folder <- view appNixecFolder

      let db = folder </> "database"

      -- First we build the database
      withArgs ["-o", db] $ do
        a <- nixPackageScript' (nixCallFile (folder </> "default.nix"))
        liftIO $ putStrLn a
        void $ nixBuild a

      let file = folder </> "database" </> "database.nix"
      b <- liftIO $ doesFileExist file
      if b
        then do
          withArgs ["-o", db] $ do
            a <- nixPackageScript' (nixCallFileWithDB file)
            liftIO $ putStrLn a
            void $ nixBuild a
        else
          return ()

    readDatabase :: App (Either (Set.Set Input) [(RuleName, FilePath)])
    readDatabase = do
      L.info "Reading database"
      dbname <- view appNixecDatabase
      f <- liftIO . tryIOError $ readDirTree return (dbname </> "rules")
      case f ^? _Right._Directory of
        Nothing -> do
          L.error $ "Expected a directory with nix files at "
            <> L.displayString dbname
            <> " got "
            <> L.displayShow f
          return (Left mempty)
        Just db -> do
          case db ^? ix ("missing.csv" :| []) ._File of
            Just fp -> do
              L.info "Found missing inputs."
              filecontent <- liftIO $ BL.readFile fp
              case Csv.decodeByName filecontent of
                Right (_, ms) ->
                  return $ Left (Set.fromList . V.toList $ ms)
                Left m ->
                  L.criticalFailure $ "Missing.csv file "
                    <> L.displayString fp
                    <> " formated incorrectly: "
                    <> L.displayShow m
            Nothing -> do
              return . Right $
                [ (ruleNameFromString n, dbname </> key)
                | (key :: String , _ ) <- itoList (getInternalFileMap db)
                , let (n, ext) = splitExtensions key
                , ext == ".nix"
                ]

{-
mkDerivation {
  packages = [  ]
  run = ''
   nixec-build ${database} -t $out
  ''
} -}
