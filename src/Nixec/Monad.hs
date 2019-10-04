{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Foldable
import Data.Maybe
import Data.Coerce
-- import qualified Data.List as List

-- cassava
import qualified Data.Csv as Csv

-- directory
import System.Directory

-- text
import qualified Data.Text as Text

-- free
import Control.Monad.Free
import Control.Monad.Free.TH

-- containers
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

-- lens
import Control.Lens hiding ((<.>))

-- filepath
import System.FilePath

-- directory
-- import System.Directory

-- bytestring
import qualified Data.ByteString.Lazy as BL

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
import Nixec.Command
import Nixec.Rule hiding (rule)
import Nixec.Nix

newtype Nixec a = Nixec
  { getFreeNixecF :: Free NixecF a }
  deriving (Functor)

data NixecF x
  = AddRule Name Rule (RuleName -> x)
  | Scope Name (Nixec Rule) (RuleName -> x)
  | forall a b. Seperate (Nixec a) (Nixec b) ((a, b) -> x)
  | forall a. Inspect Input (FilePath -> IO a) (a -> x)

deriving instance (Functor NixecF)

makeFree_ ''NixecF

addRule    :: MonadFree NixecF m => Name -> Rule -> m RuleName
scope      :: MonadFree NixecF m => Name -> Nixec Rule -> m RuleName
inspect    :: MonadFree NixecF m => Input -> (FilePath -> IO a) -> m a
seperate   :: MonadFree NixecF m => Nixec a -> Nixec b -> m (a, b)

instance Applicative Nixec where
  pure a = Nixec (return a)
  a <*> b = uncurry ($) <$> seperate a b

instance Monad Nixec where
  return = pure
  (Nixec ma) >>= m =
    Nixec (ma >>= \a -> let Nixec m' = m a in m')

instance MonadFree NixecF Nixec where
  wrap a = Nixec (Free $ coerce a)

runNixec :: Monad m => (NixecF (m a) -> m a) -> Nixec a -> m a
runNixec a = iterM a . getFreeNixecF

checkSuccess :: (MonadIO m) => RuleName -> FilePath -> m Bool
checkSuccess rn output = liftIO $ do
  (fmap Csv.decodeByName . BL.readFile $ output </> "times.csv") >>= \case
    Right (_, vn) -> do
      return $ case findOf folded (view $ statsRuleName.to (== rn)) vn of
        Just stat ->
          stat^.statsExitCode == 0
        Nothing -> False
    Left err -> do
      liftIO . putStrLn $ "Could not parse csv file " ++ output ++ ": " ++ show err
      return False

onSuccess :: RuleName -> Nixec a -> Nixec (Maybe a)
onSuccess rn n = do
  b <- inspect (RuleInput rn) $ checkSuccess rn
  if b
  then Just <$> n
  else return Nothing

data Action
  = RunAction

data Config = Config
  { _configScope     :: Scope
  , _configNixConfig :: NixConfig
  , _configTarget    :: Maybe RuleName
  , _configDryRun    :: Bool
  , _configCheck     :: Bool
  , _configAction    :: Action
  }

-- defaultConfig :: Config
-- defaultConfig = Config
--   []
--   (NixConfig "_nixec/nix" [ "overlay.nix" ] True "mkRule" ("nix-build", []))
--   Nothing
--   $ RunAction

makeClassy ''Config

parseConfig :: Parser Config
parseConfig = do
  _configScope <- pure []

  _configNixConfig <- do
    _nixOverlays <-
      fmap ("overlay.nix":) . many . strOption
      $ short 'O'
      <> long "overlays"
      <> metavar "FILE"
      <> help "Nix overlays (default: 'overlay.nix')"
      <> hidden

    _nixFolder <- pure "_nixex/nix"

    _nixBuildCommand <- pure ("nix-build", [])

    _nixMkRule <- pure "mkRule"

    _nixVerbose <- switch $
      short 'V'
      <> help "show the output of running the nixscripts."

    pure $ NixConfig {..}

  _configAction <- pure $ RunAction
    -- hsubparser . fold $
    -- [ command "list" $ info (pure ListAction)
    --   (progDesc "Print list of rules")
    -- , command "run" $ info
    --   (ExecAction .
    --   (progDesc "run derivations")
    -- ]
  _configDryRun <- switch $
    long "dry"
    <> help "don't actually exeucute any nix-scripts."

  _configCheck <- switch $
    long "check"
    <> help "check that the database is correct."

  _configTarget <-
    optional
    . fmap (ruleNameFromText . Text.pack)
    . strArgument
    $ metavar "RULE"
      <> help "the rule to run."

  pure $ Config {..}

instance HasNixConfig Config where
  nixConfig = configNixConfig

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

scopes ::
  Traversable f
  => f Name
  -> (Name -> Nixec Rule)
  -> Nixec (f RuleName)
scopes = scopesBy id

scopesBy ::
  Traversable f
  => (a -> Name)
  -> f a
  -> (a -> Nixec Rule)
  -> Nixec (f RuleName)
scopesBy an fa fn =
  forM fa $ \a -> scope (an a) (fn a)

rulesBy ::
  Traversable f
  => (a -> Name)
  -> f a
  -> (a -> RuleM ())
  -> Nixec (f RuleName)
rulesBy an fa fn =
  forM fa $ \a -> rule (an a) (fn a)

rules ::
  Traversable f
  => f Name
  -> (Name -> RuleM ())
  -> Nixec (f RuleName)
rules = rulesBy id

rule :: Name -> RuleM () -> Nixec RuleName
rule n rulem = do
  addRule n $ emptyRule &~ rulem

-- | finish a scope
collect :: RuleM () -> Nixec Rule
collect rulem = do
  return $ emptyRule &~ rulem

collectWith :: Traversable f
  => (f CommandArgument -> RuleM ())
  -> Nixec (f RuleName)
  -> Nixec Rule
collectWith rulem scps =
  scps <&> \mn -> emptyRule &~ (rulem =<< asLinks mn)

listFiles :: Input -> (FilePath -> Maybe b) -> Nixec [(b, Input)]
listFiles i fm =
  fmap catMaybes . inspect i $ \fp -> do
    content <- listDirectory fp
    return $ map (\c -> (,InInput i c) <$> fm c) content

  -- case cfg^.configAction of
  --   ListAction -> do
  --     (a, b) <- computeStrategy (makeRuleName "all"
  --     mapM_ (Map.keys )

  --  _ -> undefined
    -- ListAction -> do
    --   mapM_ (print . pretty) $ runReader (runListAction nm) cfg
    -- ExecAction name ->
    --   runReaderT (runExec name nm) cfg
    -- ToNixAction name ->
    --   runReaderT (runToNixAction name nm) cfg
    -- GetRulesAction -> do
    --   a <- runReaderT (getRuleDescriptions nm) cfg
    --   iforMOf_ ifolded a $ \rn rd -> do
    --     print (pretty rn <> line <> indent 2 (prettyList (Set.toList rd)))
    --     putStrLn ""
