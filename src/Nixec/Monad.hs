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
import Data.Maybe
import Data.Coerce
import Data.Traversable
import Control.Monad.IO.Class

-- cassava
import qualified Data.Csv as Csv

-- directory
import System.Directory

-- free
import Control.Monad.Free
import Control.Monad.Free.TH

-- lens
import Control.Lens hiding ((<.>))

-- filepath
import System.FilePath

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- Nixec
import Nixec.Data
import Nixec.Command
import Nixec.Rule hiding (rule)

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
