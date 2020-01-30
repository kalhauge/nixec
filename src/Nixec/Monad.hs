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
import           Prelude                 hiding ( log )
import           Data.Maybe
import           Data.Coerce
import           Data.Traversable
import           Control.Monad.IO.Class

-- cassava
import qualified Data.Csv                      as Csv

-- directory
import           System.Directory

-- free
import           Control.Monad.Free
import           Control.Monad.Free.TH

-- lens
import           Control.Lens            hiding ( (<.>) )

-- filepath
import           System.FilePath

-- bytestring
import qualified Data.ByteString.Lazy          as BL

-- Nixec
import           Nixec.Command
import           Nixec.Rule              hiding ( rule )

newtype Nixec a = Nixec
  { getFreeNixecF :: Free NixecF a }
  deriving (Functor)

data NixecF x
  = AddRule Name Rule (RuleName -> x)
  | Scope Name (Nixec Rule) (RuleName -> x)
  | forall a b. Seperate (Nixec a) (Nixec b) ((a, b) -> x)
  | forall a. InspectInput InputFile (FilePath -> IO a) (a -> x)

deriving instance (Functor NixecF)

makeFree_ ''NixecF

-- | Add a new rule, and return the name of the rule for future use.
addRule :: MonadFree NixecF m => Name -> Rule -> m RuleName

-- | Add a scope of the rule
scope :: MonadFree NixecF m => Name -> Nixec Rule -> m RuleName

-- | Inspect an input and allow it to be used in the computation.
inspectInput :: MonadFree NixecF m => InputFile -> (FilePath -> IO a) -> m a

-- | Run two actions in parallel
seperate :: MonadFree NixecF m => Nixec a -> Nixec b -> m (a, b)

-- | Inspect an input, but use the 'HasInputFile' typeclass instead of an
-- actual 'InputFile'
inspect
  :: (MonadFree NixecF m, HasInputFile i) => i -> (FilePath -> IO a) -> m a
inspect i = inspectInput (toInputFile i)

instance Applicative Nixec where
  pure a = Nixec (return a)
  a <*> b = uncurry ($) <$> seperate a b

instance Monad Nixec where
  return = pure
  (Nixec ma) >>= m = Nixec (ma >>= \a -> let Nixec m' = m a in m')

instance MonadFree NixecF Nixec where
  wrap a = Nixec (Free $ coerce a)

runNixec :: Monad m => (NixecF (m a) -> m a) -> Nixec a -> m a
runNixec a = iterM a . getFreeNixecF

checkSuccess :: (MonadIO m) => FilePath -> m Bool
checkSuccess output = liftIO $ do
  (fmap Csv.decodeByName . BL.readFile $ output </> "times.csv") >>= \case
    Right (_, vn) -> do
      return $ noneOf (folded . statsExitCode) (/= 0) vn
    Left err -> do
      liftIO
        .  putStrLn
        $  "Could not parse csv file "
        ++ output
        ++ ": "
        ++ show err
      return False

onSuccess :: RuleName -> Nixec a -> Nixec (Maybe a)
onSuccess rn n = do
  b <- inspect (toInputFile rn) checkSuccess
  if b then Just <$> n else return Nothing

scopes :: Traversable f => f Name -> (Name -> Nixec Rule) -> Nixec (f RuleName)
scopes = scopesBy id

scopesBy
  :: Traversable f
  => (a -> Name)
  -> f a
  -> (a -> Nixec Rule)
  -> Nixec (f RuleName)
scopesBy an fa fn = forM fa $ \a -> scope (an a) (fn a)

rulesBy
  :: Traversable f
  => (a -> Name)
  -> f a
  -> (a -> RuleM ())
  -> Nixec (f RuleName)
rulesBy an fa fn = forM fa $ \a -> rule (an a) (fn a)

rules :: Traversable f => f Name -> (Name -> RuleM ()) -> Nixec (f RuleName)
rules = rulesBy id

rule :: Name -> RuleM () -> Nixec RuleName
rule n rulem = do
  addRule n $ emptyRule &~ rulem

-- | finish a scope
collect :: RuleM () -> Nixec Rule
collect rulem = do
  return $ emptyRule &~ rulem

collectWith
  :: Traversable f
  => (f CommandArgument -> RuleM ())
  -> Nixec (f RuleName)
  -> Nixec Rule
collectWith rulem scps = scps <&> \mn -> emptyRule &~ (rulem =<< asLinks mn)

collectLinks :: Traversable f => Nixec (f RuleName) -> Nixec Rule
collectLinks scps = scps <&> \mn -> emptyRule &~ asLinks mn

listFiles
  :: HasInputFile a => a -> (FilePath -> Maybe b) -> Nixec [(b, InputFile)]
listFiles i fm = fmap catMaybes . inspect i $ \fp -> do
  content <- listDirectory fp
  return $ map (\c -> (, i <./> c) <$> fm c) content

-- TODO Be able to list subpackages.

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
