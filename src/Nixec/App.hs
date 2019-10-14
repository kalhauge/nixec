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
import System.IO.Error

-- directory
import System.Directory

-- filepath
import System.FilePath

-- dirtree
import System.DirTree

-- containers
import qualified Data.Set as Set

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- cassava
import qualified Data.Csv as Csv

-- lens
import Control.Lens hiding ((<.>))

-- optparse-applicative
import Options.Applicative hiding (Success)

-- mtl
import Control.Monad.Reader
import Control.Monad.Except

-- Nixec
import Nixec.Data
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
  where
    parseTarget = strArgument $
      metavar "DATABASE"
      <> help "The path to the database to create."


mainWithConfig :: Config -> Nixec Rule -> IO ()
mainWithConfig cfg nm = flip runReaderT cfg $ do
  L.info "Running Nixec"
  trg <- view configTarget
  buildDatabase trg (void . addRule "all" =<< nm) >>= \case
    Right () -> L.info "Success"
    Left msg -> do
      L.info "Missing computations"
      liftIO . BL.writeFile (trg </> "missing.csv") $
        Csv.encodeDefaultOrderedByName (toList msg)

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


-- mainWithConfig :: Config -> Nixec Rule -> IO ()
-- mainWithConfig cfg nm = flip runReaderT cfg $ do
--   trg <- fromMaybe (makeRuleName "all" []) <$> view configTarget

--   nixFileExist <- liftIO . doesFileExist =<< nixFile trg

--   doCheck <- view configCheck
--   when (doCheck || not nixFileExist) $ do
--     liftIO $ putStrLn "Build Database:"
--     continuouslyBuildDatabase

--   view configDryRun >>= \case
--     True -> do
--       script <- nixRuleScript trg
--       liftIO $ do
--         putStrLn "Would run following script:"
--         print script
--     False -> do
--       liftIO $ putStrLn "Build Target:"
--       nixBuildInput (RuleInput trg) >>= \case
--         Just output -> liftIO $ do
--           putStrLn "Success"
--           createDirectoryLink output "result"
--         Nothing ->
--           liftIO $ putStrLn "Failure"

--   where
--     continuouslyBuildDatabase = do
--       (fst <$> buildDatabase' (addRule "all" =<< nm)) >>= \case
--         Right _ ->
--           liftIO $ putStrLn "No Tasks..."

--         Left s -> do
--           liftIO $ do
--             putStrLn "Tasks:"
--             forM_ (Set.toAscList s) $ \rn ->
--               print ("+ " <> pretty rn)

--           doDryRun <- view configDryRun
--           unless doDryRun $ do
--             liftIO $ putStrLn "Computing tasks:"
--             nixBuildAll s >>= \case
--               Just _ ->
--                 continuouslyBuildDatabase
--               Nothing ->
--                 liftIO $ putStrLn "Failed building tasks"



-- buildDatabase' ::
--   forall env m a.
--   (HasConfig env, HasNix env m)
--   => Nixec a
--   -> m (Either (Set.Set Input) a, Map.Map RuleName Rule)
-- buildDatabase' nscript
--   = runWriterT (runExceptT . runNixec run $ nscript) where
--   run :: NixecF (MissingT m b) -> MissingT m b
--   run = \case
--     AddRule name r x -> do
--       rn <- newRuleName name
--       nixFileExist <- liftIO . doesFileExist =<< nixFile rn
--       doCheck <- view configCheck
--       unless (nixFileExist || not doCheck) $
--         writeNixRule rn r
--       tell (Map.singleton rn r)
--       view configTarget >>= \case
--         Just target
--           | rn == target ->
--               throwError (Set.singleton (RuleInput rn))
--         _ -> x rn

--     Scope name nm x -> do
--       r <- inScope name $ runNixec run nm
--       run (AddRule name r x)

--     Inspect i io na -> do
--       nixCheckBuildInput i >>= \case
--         Just output ->
--           na =<< liftIO (io output)
--         Nothing ->
--           throwError (Set.singleton i)

--     Seperate n1 n2 n -> do
--       trg <- view configTarget
--       (a, b) <- ExceptT . WriterT $ do
--         (e1, rls1) <- buildDatabase n1
--         (e2, rls2) <- buildDatabase n2
--         case (e1, e2) of
--           -- Both computed
--           (Right a, Right b) ->
--             return (Right (a, b), rls1 <> rls2)

--           -- Both failed, choose the one with the longest prefix and
--           -- report that back
--           (Left s1, Left s2) -> do
--             case trg of
--               Just target -> do
--                 let prl1 = maxPrefixLength s1
--                     prl2 = maxPrefixLength s2
--                 return $ case compare prl1 prl2 of
--                   LT -> (Left s2, rls2)
--                   GT -> (Left s1, rls1)
--                   EQ -> (Left (s1 <> s2), rls1 <> rls2)
--                 where
--                   maxInputLength = \case
--                     RuleInput f -> ruleNamePrefixLength target f
--                     _ -> 0
--                   maxPrefixLength =
--                     maximum . map maxInputLength . Set.toList
--               Nothing ->
--                 return (Left (s1 <> s2), rls1 <> rls2)

--           -- One failed, only include data to produce the fail
--           (Left s1, _) ->
--             return (Left s1, rls1)

--           -- One failed, only include data to produce the fail
--           (_, Left s2) ->
--             return (Left s2, rls2)

--       n (a, b)


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
  { _appLogger         :: L.Logger
  , _appNixecfile      :: FilePath
  , _appNixecDatabase  :: FilePath
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

  happNixecDatabase <- optional . strOption $
    long "db"
    <> help "the path to the Nixec database. Normally NIXECFILE/_nixec/database."
    <> hidden
    <> metavar "DATABASE"

  pure $ do

    _appNixecfile <- checkFileType ioAppNixecfile >>= \case
      Just (File _) ->
        makeAbsolute ioAppNixecfile
      _ ->
        flip runReaderT _appLogger . L.criticalFailure $ "Expected "
          <> L.displayString ioAppNixecfile
          <> " to be a file."

    let _appNixecDatabase =
          fromMaybe (_appNixecfile </> "_nixec/database") happNixecDatabase

    return $ AppConfig {..}

makeClassy ''AppConfig

instance L.HasLogger AppConfig where logger = appLogger

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

  db <- readDatabase >>= \case
    Just db -> return $ db
    Nothing -> do
      L.info "Could not read the database."
      calculateDatabase
      maybe (L.criticalFailure "Could not create database.") return
        =<< readDatabase

  case appCmd of
    ListRules -> do
      forM_ db $ \(rn, _) -> liftIO $ do
        putStrLn $ show rn

  where
    calculateDatabase :: App ()
    calculateDatabase = do
      L.info "Calculating the database"


    -- stepDatabase = do
    --   b <- calculateDatabaseStep
    --   missing <- lines <$> Text.readFile (b </> "missing.txt")
    --   case missing of
    --     [] -> return b
    --     m -> createMissing -> stepDatabase

    --   undefined
    --   -- nixBuild "{ pkgs ? import <nixpgks> {}}: pkgs.callPackage _nixec/default.nix {}"

    readDatabase :: App (Maybe [(RuleName, FilePath)])
    readDatabase = do
      L.info "Reading database"
      dbname <- view appNixecDatabase
      f <- liftIO . tryIOError $ readDirTree return dbname

      case f ^? _Right._Directory of
        Nothing -> do
          L.error $ "Expected a directory with nix files at "
            <> L.displayString dbname
            <> " got "
            <> L.displayShow f
          return Nothing
        Just db -> do
          return . Just $
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
