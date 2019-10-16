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

-- vector
import qualified Data.Vector as V

-- containers
import qualified Data.Set as Set

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- cassava
import qualified Data.Csv as Csv

-- lens
import Control.Lens hiding ((<.>), argument)

-- optparse-applicative
import Options.Applicative hiding (Success)

-- prettyprinter
-- import Data.Text.Prettyprint.Doc

-- mtl
import Control.Monad.Reader

-- Nixec
import Nixec.Data
import Nixec.Nix
import qualified Nixec.Logger as L

data AppCommand
  = ListRules
  | RunRules [RuleName]
  deriving (Show, Eq)

parseAppCommand :: Parser AppCommand
parseAppCommand =
  hsubparser . fold $
  [ command "list" $
    info
    (pure ListRules)
    (progDesc "list the possible targets.")
  , command "run" $
     info
     ( fmap RunRules
       . some
       . argument (maybeReader $ Just . ruleNameFromString)
       $ metavar "RULE .."
       <> help "the rules to run."
     )
     (progDesc "run a list of rules.")
  ]

data AppConfig = AppConfig
  { _appLogger         :: !L.Logger
  , _appCheck          :: !Bool
  , _appNixecfile      :: !FilePath
  , _appNixecFolder    :: !FilePath
  , _appDatabase       :: !FilePath
  , _appNixConfig      :: !NixConfig
  } deriving (Show, Eq)


parseAppConfig :: Parser (L.Logger -> IO AppConfig)
parseAppConfig = do
  _appCheck <-
    switch $
    long "check"
    <> help "recalculate the database."
    <> hidden

  ioAppNixecfile <- strOption $
    short 'f'
    <> long "file"
    <> help "the path to the Nixecfile.hs."
    <> value "./Nixecfile.hs"
    <> hidden
    <> metavar "NIXECFILE"
    <> showDefault

  nixArgs <- many . strOption $
    short 'n'
    <> help "nix arguments"
    <> value "./Nixecfile.hs"
    <> hidden
    <> metavar "ARG"

  mappNixecFolder <- optional . strOption $
    long "nixec-folder"
    <> help "the path to the Nixec database. Normally NIXECFILE/../_nixec"
    <> hidden
    <> metavar "NIXECFOLDER"

  pure $ \_appLogger -> do

    _appNixecfile <- checkFileType ioAppNixecfile >>= \case
      Just (File _) ->
        makeAbsolute ioAppNixecfile
      _ ->
        flip runReaderT _appLogger . L.criticalFailure $ "Expected "
          <> L.displayString ioAppNixecfile
          <> " to be a file."

    let
      _appNixecFolder = fromMaybe (takeDirectory _appNixecfile </> "_nixec") mappNixecFolder
      _appDatabase = _appNixecFolder </> "database"

      _appNixConfig =
        let
          _nixOverlays = [ _appNixecFolder </> "overlay.nix" ]
          _nixVerbose = True
          _nixBuildCommand = ("nix-build", nixArgs)
        in NixConfig {..}


    return $ AppConfig {..}

makeClassy ''AppConfig

appRulesFolder :: HasAppConfig env => Getter env FilePath
appRulesFolder =
  appDatabase . to (</> "rules")

instance L.HasLogger AppConfig where logger = appLogger
instance HasNixConfig AppConfig where nixConfig = appNixConfig

type App = ReaderT AppConfig IO

app :: IO ()
app = do
  putStrLn "Hello, World!"

  (logger, ioCfg, appCmd) <- execParser $
    info ((liftA3 (,,) L.parseLogger parseAppConfig parseAppCommand)
          <**> helper) (header "nixec")

  runReaderT (L.debug $ "Read command line arguments. ") logger
  cfg <- ioCfg logger
  runReaderT (L.debug $ "Read config: " <> L.displayShow cfg) cfg
  runReaderT (runapp appCmd) cfg

runapp :: AppCommand -> ReaderT AppConfig IO ()
runapp appCmd = do
  nixecfile <- view appNixecfile
  cfg <- view appConfig
  L.info "Started Nixec."
  L.info  $ "Found Nixecfile: " <> L.displayString nixecfile

  check <- view appCheck
  when check $ calculateDatabase
 
  db <- readDatabase >>= \case
    Right db -> do
      L.info "Database succesfully read."
      return $ db
    Left missing -> do
      L.info $ "Could not read the database: "
        <> if Set.null missing
        then "no database."
        else "missing " <> L.displayShow (Set.size missing) <> " dependencies."
      calculateDatabase
      L.criticalFailure "Could not create database."

  case appCmd of
    ListRules -> do
      forM_ db $ \(rn, _) -> liftIO $ do
        putStrLn $ ruleNameToString rn

    RunRules rns -> do
      rules <- view appRulesFolder
      nixBuildRules rules rns >>= \case
        Just _ -> L.info "success"
        Nothing ->
          L.criticalFailure "could not build the rules."
     

  where
    calculateDatabase :: App ()
    calculateDatabase = L.phase "compute database" $ do
      L.info "Calculating the database"

      folder <- view appNixecFolder
      let db = folder </> "database"
      buildFileOrDie db (folder </> "default.nix")
      iterate db

    buildFileOrDie db file =
      withArgs ["-o", db] $ do
        a <- nixPackageScript' (nixCallFile file)
        nixBuild a >>= \case
          Just x -> return ()
          Nothing ->
            L.criticalFailure "Could not build database."

    iterate db = do
      let file = db </> "database.nix"
      b <- liftIO $ doesFileExist file
      when b (buildFileOrDie db file >> iterate db)

    readDatabase :: App (Either (Set.Set Input) [(RuleName, FilePath)])
    readDatabase = L.phase "read database" $ do
      L.info "Reading database"
      rules <- view appRulesFolder
      f <- liftIO . tryIOError $ readDirTree return rules
      case f ^? _Right._Directory of
        Nothing -> do
          L.error $ "Expected a directory with nix files at "
            <> L.displayString rules
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
                [ (ruleNameFromString n, rules </> key)
                | (key :: String , _ ) <- itoList (getInternalFileMap db)
                , let (n, ext) = splitExtensions key
                , ext == ".nix"
                ]
