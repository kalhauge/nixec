{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Monoid
import Data.Foldable
import Data.Coerce
-- import Control.Monad.Fail
import qualified Data.List as List

-- cassava
import qualified Data.Csv as Csv

-- typed-process
import System.Process.Typed

-- text
import qualified Data.Text as Text

-- free
import Control.Monad.Free
import Control.Monad.Free.TH

-- containers
-- import Data.Tree
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Function

-- lens
import Control.Lens hiding ((<.>))

-- filepath
import System.FilePath

-- directory
import System.Directory

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- optparse-applicative
import Options.Applicative hiding (Success)

-- mtl
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

-- transformers
import Control.Monad.Trans.Maybe

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- Nixec
import Nixec.Data
import Nixec.Rule hiding (rule)
import Nixec.Nix

newtype Nixec a = Nixec
  { getFreeNixecF :: Free NixecF a }
  deriving (Functor)

data NixecF x
  = AddRule Name Rule (RuleName -> x)
  | Scope Name (Nixec Rule) (RuleName -> x)
  | forall a b. Seperate (Nixec a) (Nixec b) ((a, b) -> x)
  | forall a. OnSuccess RuleName (Nixec a) (a -> x) x

deriving instance (Functor NixecF)

makeFree_ ''NixecF

addRule    :: MonadFree NixecF m => Name -> Rule -> m RuleName
scope      :: MonadFree NixecF m => Name -> Nixec Rule -> m RuleName
onSuccess  :: MonadFree NixecF m => RuleName -> Nixec a -> m (Maybe a)
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

rule :: Name -> RuleM () -> Nixec RuleName
rule n rulem = do
  addRule n $ emptyRule &~ rulem

-- | finish a scope
collect :: RuleM () -> Nixec Rule
collect rulem = do
  return $ emptyRule &~ rulem

data Action
  = ListAction
  | ToNixAction RuleName
  | ExecAction RuleName
  | GetRulesAction

data Config = Config
  { _configScope    :: Scope
  , _configFolder   :: FilePath
  , _configOverlays :: [ FilePath ]
  , _configMkRule   :: Package
  , _configAction   :: Action
  }

defaultConfig :: Config
defaultConfig =
  Config
  [] "_nixec"
  [ "overlay.nix" ]
  "mkRule"
  $ GetRulesAction

makeClassy ''Config

parseConfig :: Parser Config
parseConfig = do
  _configOverlays <-
    fmap ("overlay.nix":) . many . strOption
    $ short 'O'
      <> long "overlays"
      <> metavar "FILE"
      <> help "Nix overlays (default: 'overlay.nix')"
      <> hidden

  _configAction <-
    hsubparser . fold $
    [ command "list" $ info (pure ListAction)
      (progDesc "Print list of rules")
    , command "run" $ info
      (ExecAction . ruleNameFromText . Text.pack <$>
        strArgument (
          metavar "RULE"
            <> help "the rule to run."
          )
      )
      (progDesc "run derivations")
    ]

  pure $
    defaultConfig
    & configOverlays .~ _configOverlays
    & configAction .~ _configAction
 

newRuleName ::
  (HasConfig env, MonadReader env m)
  => Name
  -> m RuleName
newRuleName name = do
  makeRuleName name <$> view configScope

runToNixAction ::
  forall env m. (MonadReader env m, HasConfig env, MonadIO m)
  => RuleName
  -> Nixec Rule
  -> m ()
runToNixAction target = void . runNixec run where
  run :: NixecF (m a) -> m a
  run = \case
    AddRule name r x -> do
      rn <- newRuleName name
      if rn == target
        then do
        liftIO $ print (ruleToNix rn r)
        x rn
        else x rn
    Scope name nm x -> do
      n <- view configScope
      r <- local (configScope .~ name:n) $ runNixec run nm
      run (AddRule name r x)
    OnSuccess rn nm na _ -> do
      _ <- runNixec run nm
      na (error $ "The success of '" ++ show (pretty rn) ++ "'cannot effect the number of rules.")
    Seperate n1 n2 n -> do
      a <- runNixec run n1
      b <- runNixec run n2
      n (a, b)

runListAction
  :: (MonadReader env m, HasConfig env)
  => Nixec Rule
  -> m [RuleName]
runListAction =
  fmap (flip appEndo [makeRuleName "all" []]) . execWriterT . runNixec run
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
        r <- local (configScope .~ name:n) $ runNixec run nm
        run (AddRule name r x)
      OnSuccess rn nm na _ -> do
        _ <- runNixec run nm
        na (error $ "The success of '" ++ show (pretty rn) ++ "'cannot effect the number of rules.")
      Seperate n1 n2 n -> do
        a <- runNixec run n1
        b <- runNixec run n2
        n (a, b)


type RuleDescription = Set.Set RuleName

type Rules = Map.Map RuleName RuleDescription

data RuleState = RuleState
  { _rules      :: Rules
  , _ruleUses   :: Set.Set RuleName
  }

makeLenses ''RuleState

emptyRuleState :: RuleState
emptyRuleState =
  (RuleState Map.empty Set.empty)

inScope :: (MonadReader env m, HasConfig env) => Name -> m a -> m a
inScope name =
  local $ configScope %~ (name:)

getRuleDescriptions ::
  forall m env. (MonadReader env m, HasConfig env)
  => Nixec Rule
  -> m Rules
getRuleDescriptions nscript = do
  x <- execStateT (runNixec run (addRule "all" =<< nscript)) emptyRuleState
  return $ x^.rules
  where
    run :: NixecF ((StateT RuleState m) a) -> (StateT RuleState m) a
    run = \case
      AddRule name _ x -> do
        rn <- newRuleName name
        known <- use ruleUses
        rules.at rn .= Just known
        x rn
      Scope name nm x -> do
        r <- inScope name $ runNixec run nm
        run (AddRule name r x)
      OnSuccess rn na xa x -> do
        ruleUses %= Set.insert rn
        a <- runNixec run na
        StateT $ \s -> do
          (_, s1) <- runStateT (xa a) s
          (_, s2) <- runStateT x s
          return ( error "I hope this value is not used"
                 , RuleState
                   ((Map.unionWith (<>) `on` view rules) s1 s2)
                   ((Set.union `on` view ruleUses) s1 s2)
                 )
      Seperate n1 n2 x -> do
        (a, b) <- StateT $ \s -> do
          let s' = emptyRuleState & ruleUses .~ s^.ruleUses
          (a, s1) <- runStateT (runNixec run n1) s'
          (b, s2) <- runStateT (runNixec run n2) s'
          return ( (a, b)
                 , RuleState
                   (List.foldr1
                     (Map.unionWithKey (\k _ _ -> error ("Rule name used twice: " ++ show (pretty k))))
                     [view rules s'' | s'' <- [s, s1, s2]]
                   )
                   ((Set.union `on` view ruleUses) s1 s2)
                 )
        x (a, b)

runExec ::
  forall env m.
  (MonadReader env m, HasConfig env, MonadIO m)
  => RuleName
  -> Nixec Rule
  -> m ()
runExec target nscript = do
  nixDir <- (</> "nix") <$> view configFolder
  liftIO $ do
    createDirectoryIfMissing True nixDir

  rs <- getRuleDescriptions nscript

  case rs^.at target of
    Just deps -> do
      liftIO . print $ pretty target
        <> " depends on:" <> line
        <> indent 2 (vcat . map pretty $ Set.toList deps)

      (mx, sx) <- findMissing target nscript

      liftIO $ do
        putStrLn ""
        putStrLn "Ensure nixscript is updated:"
        forM_ (List.sort $ Map.keys mx) $ \rn -> do
          print (" - " <> pretty rn)

        putStrLn ""

        putStrLn "Build the following scripts:"
        forM_ (List.sort $ Set.toList sx) $ \rn -> do
          print (" - " <> pretty rn)

        putStrLn ""

      liftIO $ putStrLn "Writing rules:"
      iforM_ mx writeNixRule

      liftIO $ putStrLn "Running dependencies:"
      nixBuildAll sx
    
    Nothing -> liftIO $ do
      print (pretty target <+> "not a rule. Choose from:")
      forM_ (List.sort $ Map.keys rs) $ \r -> do
        print (" - " <> pretty r)


type MissingT m a=
  MaybeT (WriterT (Map.Map RuleName Rule, Set.Set RuleName) m) a

findMissing ::
  forall env m.
  (MonadReader env m, HasConfig env, MonadIO m)
  => RuleName
  -> Nixec Rule
  -> m (Map.Map RuleName Rule, Set.Set RuleName)
findMissing target nscript = execWriterT (runMaybeT . runNixec run $ nscript) where
  run :: NixecF (MissingT m a) -> MissingT m a
  run = \case
    AddRule name r x -> do
      rn <- newRuleName name
      tell (Map.singleton rn r, Set.empty)
      x rn

    Scope name nm x -> do
      r <- inScope name $ do
        guard =<< ruleNameInScope target <$> view configScope
        runNixec run nm
      run (AddRule name r x)

    OnSuccess rn nm na n -> do
      output <- dropExtension <$> nixFile rn
      exist <- liftIO $ doesPathExist output
      if not exist
        then do
        tell (Map.empty, Set.singleton rn)
        mzero
        else do
        success <- checkSuccess output rn
        if success
          then runNixec run nm >>= na
          else n

    Seperate n1 n2 n -> do
      void (runNixec run n1) <|> void (runNixec run n2)
      n

checkSuccess :: (MonadIO m) => FilePath -> RuleName -> m Bool
checkSuccess output rn = do
  (liftIO . fmap Csv.decodeByName . BL.readFile $ output </> "times.csv") <&> \case
    Right (_, vn) -> do
      case findOf folded (view $ statsRuleName.to (== rn)) vn of
        Just stat ->
          stat^.statsExitCode == 0
        Nothing -> False
    _ -> False

writeNixRule :: (MonadIO m, HasConfig env, MonadReader env m)
  => RuleName
  -> Rule
  -> m ()
writeNixRule rn r = do
  file <- nixFile rn
  liftIO . writeFile file $ show (ruleToNix rn r)

nixFile :: (MonadReader env m, HasConfig env) => RuleName -> m FilePath
nixFile rn =
  view configFolder <&> \cnf ->
  cnf </> "nix" </> show (ruleNameToNix rn) <.> "nix"

nixBuild :: (MonadIO m, HasConfig env, MonadReader env m)
  => FilePath
  -> RuleName
  -> m ()
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

nixBuildAll :: (MonadIO m, HasConfig env, MonadReader env m, Foldable f)
  => f RuleName
  -> m ()
nixBuildAll rns = do
  forM_ rns $ \rn -> do
    output <- dropExtension <$> nixFile rn
    nixBuild output rn

runExecAction ::
  forall env m.
  (MonadReader env m, HasConfig env, MonadIO m)
  => RuleName
  -> Nixec Rule
  -> m ()
runExecAction target nscript = do
  nixDir <- (</> "nix") <$> view configFolder
  liftIO $ do
    createDirectoryIfMissing True nixDir

  r <- runNixec run nscript

  if target == ruleNameFromText "all"
    then writeNixRule target r
    else return ()

  nixBuild "result" target

  where


    run :: NixecF (m a) -> m a
    run = \case
      AddRule name r mn -> do
        rn <- newRuleName name
        writeNixRule rn r
        mn rn

      Scope name nm x -> do
        n <- view configScope
        r <- local (configScope .~ name:n) $ runNixec run nm
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
          $ output </> "times.csv"
        case findOf folded (view $ statsRuleName.to (== rn)) vn of
          Just stat
            | stat ^.statsExitCode == 0 -> do
              a <- runNixec run nm
              na a
          _ -> n
      Seperate n1 n2 n -> do
        a <- runNixec run n1
        b <- runNixec run n2
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
mainWithConfig cfg nm = do
  case cfg^.configAction of
    ListAction -> do
      mapM_ (print . pretty) $ runReader (runListAction nm) cfg
    ExecAction name ->
      runReaderT (runExec name nm) cfg
    ToNixAction name ->
      runReaderT (runToNixAction name nm) cfg
    GetRulesAction -> do
      a <- runReaderT (getRuleDescriptions nm) cfg
      iforMOf_ ifolded a $ \rn rd -> do
        print (pretty rn <> line <> indent 2 (prettyList (Set.toList rd)))
        putStrLn ""
