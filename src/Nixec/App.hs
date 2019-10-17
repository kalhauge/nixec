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
import Nixec.Nix
import Nixec.Rule
import qualified Nixec.Logger as L

data AppCommand
  = ListCmd
  | RunCmd
  | ToNixCmd
  | ToDrvCmd
  deriving (Show, Eq)

parseAppCommand :: Parser AppCommand
parseAppCommand =
  subparser . fold $
  [ command "list" (info (pure ListCmd) (progDesc "lists the rules."))
  , command "run" (info (pure RunCmd) (progDesc "runs the rules."))
  , command "to-nix" (info (pure ToNixCmd) (progDesc "output the nix script which can compute the rules."))
  , command "to-drv" (info (pure ToDrvCmd) (progDesc "output the derivation which can be used to compute the rules."))
  ]

data AppConfig = AppConfig
  { _appLogger         :: !L.Logger
  , _appCheck          :: !Bool
  , _appNixecfile      :: !FilePath
  , _appNixecFolder    :: !FilePath
  , _appDatabase       :: !FilePath
  , _appNixConfig      :: !NixConfig
  , _appRuleSelector   :: ![RuleName]
  } deriving (Show, Eq)


parseAppConfig :: Parser (L.Logger -> IO AppConfig)
parseAppConfig = do
  _appRuleSelector <-
    many
    . argument (maybeReader $ Just . ruleNameFromString)
    $ metavar "RULE.."
    <> help "a list of rules."
   
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
    <> hidden
    <> metavar "ARG"

  _nixSystem <- optional . strOption $
    long "system"
    <> help ("the system to run on. Defaults to your system."
        <> " Options include \"x86_64-linux\", \"x86_64-darwin\", or \"i686-linux\"."
        <> " If you have configured builders in nix, it will try to use those."
            )
    <> hidden
    <> metavar "SYSTEM"


  mappNixecFolder <- optional . strOption $
    long "nixec"
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
      _appNixConfig = NixConfig
        { _nixOverlays = [ _appNixecFolder </> "overlay.nix" ]
        , _nixBuildCommand = ("nix-build", nixArgs)
        , _nixSystem = _nixSystem
        }

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
  (appCmd, ioCfg, logger) <- execParser $
    info ((liftA3 (,,) parseAppCommand parseAppConfig L.parseLogger)
          <**> helper) (header "nixec")

  runReaderT (L.debug $ "Read command line arguments. ") logger
  cfg <- ioCfg logger
  runReaderT (L.debug $ "Read config: " <> L.displayShow cfg) cfg
  runReaderT (runapp appCmd) cfg

runapp :: AppCommand -> ReaderT AppConfig IO ()
runapp appCmd = do
  nixecfile <- view appNixecfile
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
    ListCmd -> L.phase "list" $ do
      forM_ db $ \(rn, _) -> liftIO $ do
        putStrLn $ ruleNameToString rn

    RunCmd -> L.phase "run" $ do
      rules <- view appRulesFolder
      rns <- view appRuleSelector
      nixBuildRules rules rns >>= \case
        Just s -> do
          L.info "success"
          liftIO $ putStrLn s
        Nothing ->
          L.criticalFailure "could not build the rules."

    ToNixCmd -> L.phase "to-nix" $ do
      rules <- view appRulesFolder
      rns <- view appRuleSelector
      printExprWithPkgsAndOverlays (rulesExpr rules rns)

    ToDrvCmd -> L.phase "to-drv" $ do
      rules <- view appRulesFolder
      rns <- view appRuleSelector
      nixInstantiateWithPkgsAndOverlays (rulesExpr rules rns) >>= \case
        Just s -> do
          L.info "success"
          liftIO $ putStrLn s
        Nothing ->
          L.criticalFailure "could not instantiate the rules."

  where
    calculateDatabase :: App ()
    calculateDatabase = L.phase "compute db" $ do
      L.info "Calculating the database"

      folder <- view appNixecFolder
      let db = folder </> "database"
      buildFileOrDie db (folder </> "default.nix")
      go db
      where
        go db = do
          let _file = db </> "database.nix"
          b <- liftIO $ doesFileExist _file
          when b (buildFileOrDie db _file >> go db)

        buildFileOrDie db _file =
          withArgs ["-o", db] $ do
            nixBuildWithPkgsAndOverlays (callFileExpr _file []) >>= \case
              Just _ -> return ()
              Nothing ->
                L.criticalFailure "Could not build database."


    readDatabase :: App (Either (Set.Set InputFile) [(RuleName, FilePath)])
    readDatabase = L.phase "read db" $ do
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
                  L.criticalFailure $ "missing.csv file "
                    <> L.displayString fp
                    <> " formated incorrectly: "
                    <> L.displayShow m
            Nothing -> do
              return . Right $
                [ (ruleNameFromString n, rules </> p)
                | (key, _ ) <- itoList db
                , let p = fileKeyToPath $ fromForestFileKey key
                , let (n, ext) = splitExtensions p
                , ext == ".rule.nix"
                ]
