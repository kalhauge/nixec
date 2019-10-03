{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
-- import Data.Monoid
import Data.Foldable
import Data.Coerce
import Data.String
-- import Control.Monad.Fail
import qualified Data.List as List

-- cassava
import qualified Data.Csv as Csv


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
-- import Data.Functor

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
import Nixec.Rule hiding (rule)
import Nixec.Nix

newtype Nixec a = Nixec
  { getFreeNixecF :: Free NixecF a }
  deriving (Functor)

data NixecF x
  = AddRule Name Rule (RuleName -> x)
  | Scope Name (Nixec Rule) (RuleName -> x)
  | forall a b. Seperate (Nixec a) (Nixec b) ((a, b) -> x)
  | forall a. Inspect RuleName (FilePath -> IO a) (a -> x)

deriving instance (Functor NixecF)

makeFree_ ''NixecF

addRule    :: MonadFree NixecF m => Name -> Rule -> m RuleName
scope      :: MonadFree NixecF m => Name -> Nixec Rule -> m RuleName
inspect    :: MonadFree NixecF m => RuleName -> (FilePath -> IO a) -> m a
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


checkSuccess :: (MonadIO m) => RuleName -> FilePath -> m Bool
checkSuccess rn output = liftIO $ do
  (fmap Csv.decodeByName . BL.readFile $ output </> "times.csv") >>= \case
    Right (_, vn) -> do
      print vn
      return $ case findOf folded (view $ statsRuleName.to (== rn)) vn of
        Just stat ->
          stat^.statsExitCode == 0
        Nothing -> False
    Left err -> do
      print err
      return False

onSuccess :: RuleName -> Nixec a -> Nixec (Maybe a)
onSuccess rn n = do
  b <- inspect rn $ \fp -> do
    a <- checkSuccess rn fp
    print ("Tested " <> pretty rn <> " result: " <> viaShow a)
    print ("Check " <> fromString fp <> " for more." :: Doc ())
    return a
  if b
  then Just <$> n
  else return Nothing

data Action
  = ListAction
  | ToNixAction RuleName
  | ExecAction RuleName
  | GetRulesAction

data Config = Config
  { _configScope     :: Scope
  , _configNixConfig :: NixConfig
  , _configAction    :: Action
  }

defaultConfig :: Config
defaultConfig =
  Config
  []
  (NixConfig "_nixec/nix" [ "overlay.nix" ])
  $ GetRulesAction

makeClassy ''Config

parseConfig :: Parser Config
parseConfig = do
  _configNixConfig <- do
    _nixOverlays <-
      fmap ("overlay.nix":) . many . strOption
      $ short 'O'
      <> long "overlays"
      <> metavar "FILE"
      <> help "Nix overlays (default: 'overlay.nix')"
      <> hidden

    _nixFolder <- pure "_nixex/nix"

    pure $ NixConfig {..}

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
    & configNixConfig .~ _configNixConfig
    & configAction .~ _configAction

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
  ExceptT (Set.Set RuleName) (WriterT (Map.Map RuleName Rule) m) a

computeStrategy ::
  forall env m a.
  (HasConfig env, HasNix env m)
  => RuleName
  -> Nixec a
  -> m (Either (Set.Set RuleName) a, Map.Map RuleName Rule)
computeStrategy target nscript
  = runWriterT (runExceptT . runNixec run $ nscript) where
  run :: NixecF (MissingT m b) -> MissingT m b
  run = \case
    AddRule name r x -> do
      rn <- newRuleName name
      writeNixRule rn r
      tell (Map.singleton rn r)
      if rn == target
        then throwError (Set.singleton rn)
        else x rn

    Scope name nm x -> do
      r <- inScope name $ runNixec run nm
      run (AddRule name r x)

    Inspect rn io na -> do
      nixCheckBuild rn >>= \case
        Just output ->
          na =<< liftIO (io output)
        Nothing ->
          throwError (Set.singleton rn)

    Seperate n1 n2 n -> do
      (a, b) <- ExceptT . WriterT $ do
        (e1, rls1) <- computeStrategy target n1
        (e2, rls2) <- computeStrategy target n2
        case (e1, e2) of
          -- Both computed
          (Right a, Right b) ->
            return (Right (a, b), rls1 <> rls2)

          -- Both failed, choose the one with the longest prefix and
          -- report that back
          (Left s1, Left s2) -> do
            let prl1 = maxPrefixLength s1
                prl2 = maxPrefixLength s2
            return $ case compare prl1 prl2 of
              LT -> (Left s2, rls2)
              GT -> (Left s1, rls1)
              EQ -> (Left (s1 <> s2), rls1 <> rls2)
            where
              maxPrefixLength =
                maximum . map (ruleNamePrefixLength target) . Set.toList

          -- One failed, only include data to produce the fail
          (Left s1, _) ->
            return (Left s1, rls1)

          -- One failed, only include data to produce the fail
          (_, Left s2) ->
            return (Left s2, rls2)

      n (a, b)

-- runToNixAction ::
--   forall env m. (MonadReader env m, HasConfig env, MonadIO m)
--   => RuleName
--   -> Nixec Rule
--   -> m ()
-- runToNixAction target = void . runNixec run where
--   run :: NixecF (m a) -> m a
--   run = \case
--     AddRule name r x -> do
--       rn <- newRuleName name
--       if rn == target
--         then do
--         liftIO $ print (ruleToNix rn r)
--         x rn
--         else x rn
--     Scope name nm x -> do
--       n <- view configScope
--       r <- local (configScope .~ name:n) $ runNixec run nm
--       run (AddRule name r x)
--     OnSuccess rn nm na _ -> do
--       _ <- runNixec run nm
--       na (error $ "The success of '" ++ show (pretty rn) ++ "'cannot effect the number of rules.")
--     Seperate n1 n2 n -> do
--       a <- runNixec run n1
--       b <- runNixec run n2
--       n (a, b)

-- runListAction
--   :: (MonadReader env m, HasConfig env)
--   => Nixec Rule
--   -> m [RuleName]
-- runListAction =
--   fmap (flip appEndo [makeRuleName "all" []]) . execWriterT . runNixec run
--   where
--     run ::
--       (HasConfig env, MonadReader env m', MonadWriter (Endo [RuleName]) m')
--       => NixecF (m' a)
--       -> m' a
--     run = \case
--       AddRule name _ x -> do
--         rn <- newRuleName name
--         tell $ Endo (rn:)
--         x rn
--       Scope name nm x -> do
--         n <- view configScope
--         r <- local (configScope .~ name:n) $ runNixec run nm
--         run (AddRule name r x)
--       OnSuccess rn nm na _ -> do
--         _ <- runNixec run nm
--         na (error $ "The success of '" ++ show (pretty rn) ++ "'cannot effect the number of rules.")
--       Seperate n1 n2 n -> do
--         a <- runNixec run n1
--         b <- runNixec run n2
--         n (a, b)


-- type RuleDescription = Set.Set RuleName

-- type Rules = Map.Map RuleName RuleDescription

-- data RuleState = RuleState
--   { _rules      :: Rules
--   , _ruleUses   :: Set.Set RuleName
--   }

-- makeLenses ''RuleState

-- emptyRuleState :: RuleState
-- emptyRuleState =
--   (RuleState Map.empty Set.empty)

-- getRuleDescriptions ::
--   forall m env. (MonadReader env m, HasConfig env)
--   => Nixec Rule
--   -> m Rules
-- getRuleDescriptions nscript = do
--   x <- execStateT (runNixec run (addRule "all" =<< nscript)) emptyRuleState
--   return $ x^.rules
--   where
--     run :: NixecF ((StateT RuleState m) a) -> (StateT RuleState m) a
--     run = \case
--       AddRule name _ x -> do
--         rn <- newRuleName name
--         known <- use ruleUses
--         rules.at rn .= Just known
--         x rn
--       Scope name nm x -> do
--         r <- inScope name $ runNixec run nm
--         run (AddRule name r x)
--       OnSuccess rn na xa x -> do
--         ruleUses %= Set.insert rn
--         a <- runNixec run na
--         StateT $ \s -> do
--           (_, s1) <- runStateT (xa a) s
--           (_, s2) <- runStateT x s
--           return ( error "I hope this value is not used"
--                  , RuleState
--                    ((Map.unionWith (<>) `on` view rules) s1 s2)
--                    ((Set.union `on` view ruleUses) s1 s2)
--                  )
--       Seperate n1 n2 x -> do
--         (a, b) <- StateT $ \s -> do
--           let s' = emptyRuleState & ruleUses .~ s^.ruleUses
--           (a, s1) <- runStateT (runNixec run n1) s'
--           (b, s2) <- runStateT (runNixec run n2) s'
--           return ( (a, b)
--                  , RuleState
--                    (List.foldr1
--                      (Map.unionWithKey (\k _ _ -> error ("Rule name used twice: " ++ show (pretty k))))
--                      [view rules s'' | s'' <- [s, s1, s2]]
--                    )
--                    ((Set.union `on` view ruleUses) s1 s2)
--                  )
--         x (a, b)

-- runExec ::
--   forall env m.
--   (MonadReader env m, HasConfig env, MonadIO m)
--   => RuleName
--   -> Nixec Rule
--   -> m ()
-- runExec target nscript = do
--   nixDir <- (</> "nix") <$> view configFolder
--   liftIO $ do
--     createDirectoryIfMissing True nixDir

--   rs <- getRuleDescriptions nscript

--   case rs^.at target of
--     Just deps -> do
--       liftIO . print $ pretty target
--         <> " depends on:" <> line
--         <> indent 2 (vcat . map pretty $ Set.toList deps)

--       (mx, sx) <- findMissing target nscript

--       liftIO $ do
--         putStrLn ""
--         putStrLn "Ensure nixscript is updated:"
--         forM_ (List.sort $ Map.keys mx) $ \rn -> do
--           print (" - " <> pretty rn)

--         putStrLn ""

--         putStrLn "Build the following scripts:"
--         forM_ (List.sort $ Set.toList sx) $ \rn -> do
--           print (" - " <> pretty rn)

--         putStrLn ""

--       liftIO $ putStrLn "Writing rules:"
--       iforM_ mx writeNixRule

--       liftIO $ putStrLn "Running dependencies:"
--       nixBuildAll sx
    
--     Nothing -> liftIO $ do
--       print (pretty target <+> "not a rule. Choose from:")
--       forM_ (List.sort $ Map.keys rs) $ \r -> do
--         print (" - " <> pretty r)


-- type MissingT m a=
--   MaybeT (WriterT (Map.Map RuleName Rule, Set.Set RuleName) m) a

-- findMissing ::
--   forall env m.
--   (MonadReader env m, HasConfig env, MonadIO m)
--   => RuleName
--   -> Nixec Rule
--   -> m (Map.Map RuleName Rule, Set.Set RuleName)
-- findMissing target nscript = execWriterT (runMaybeT . runNixec run $ nscript) where
--   run :: NixecF (MissingT m a) -> MissingT m a
--   run = \case
--     AddRule name r x -> do
--       rn <- newRuleName name
--       tell (Map.singleton rn r, Set.empty)
--       x rn

--     Scope name nm x -> do
--       r <- inScope name $ do
--         guard =<< ruleNameInScope target <$> view configScope
--         runNixec run nm
--       run (AddRule name r x)

--     OnSuccess rn nm na n -> do
--       output <- dropExtension <$> nixFile rn
--       exist <- liftIO $ doesPathExist output
--       if not exist
--         then do
--         tell (Map.empty, Set.singleton rn)
--         mzero
--         else do
--         success <- checkSuccess output rn
--         if success
--           then runNixec run nm >>= na
--           else n

--     Seperate n1 n2 n -> do
--       void (runNixec run n1) <|> void (runNixec run n2)
--       mzero

-- runExecAction ::
--   forall env m.
--   (MonadReader env m, HasConfig env, MonadIO m)
--   => RuleName
--   -> Nixec Rule
--   -> m ()
-- runExecAction target nscript = do
--   nixDir <- (</> "nix") <$> view configFolder
--   liftIO $ do
--     createDirectoryIfMissing True nixDir

--   r <- runNixec run nscript

--   if target == ruleNameFromText "all"
--     then writeNixRule target r
--     else return ()

--   nixBuild "result" target

--   where


--     run :: NixecF (m a) -> m a
--     run = \case
--       AddRule name r mn -> do
--         rn <- newRuleName name
--         writeNixRule rn r
--         mn rn

--       Scope name nm x -> do
--         n <- view configScope
--         r <- local (configScope .~ name:n) $ runNixec run nm
--         rn <- newRuleName name
--         writeNixRule rn r
--         x rn

--       OnSuccess rn nm na n -> do
--         output <- dropExtension <$> nixFile rn
--         nixBuild output rn
--         Right (_, vn) <-
--           liftIO
--           . fmap Csv.decodeByName
--           . BL.readFile
--           $ output </> "times.csv"
--         case findOf folded (view $ statsRuleName.to (== rn)) vn of
--           Just stat
--             | stat ^.statsExitCode == 0 -> do
--               a <- runNixec run nm
--               na a
--           _ -> n
--       Seperate n1 n2 n -> do
--         a <- runNixec run n1
--         b <- runNixec run n2
--         n (a, b)

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
  (a, rls) <-
    computeStrategy (ruleNameFromText "urlfc5806b04b_wlu_mstr_leveldb_java:cfr:class") nm

  liftIO $ do
    putStrLn "Known rules:"
    forM_ (List.sort $ Map.keys rls) $ \rn ->
      print ("- " <> pretty rn)
    putStrLn ""

  case a of
    Right _ ->
      liftIO $ putStrLn "No Tasks..."

    Left s -> do
      liftIO $ putStrLn "Tasks:"
      liftIO . forM_ (Set.toAscList s) $ \rn ->
        print ("+ " <> pretty rn)

      nixBuildAll s


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
