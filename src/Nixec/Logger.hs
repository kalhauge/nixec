{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixec.Logger
  ( Logger (..)
  , HasLogger (..)
  , parseLogger

  -- * Writing messages
  , log
  , debug, info, notice, warning, error, critical
  , Priority (..)

  , timedPhase
  , phase


  -- * Building messages
  , Builder
  , Display (..)
  , displayf
  , displayShow
  , displayString
  , displayText
  , displayLazyText

  -- * Utils
  , criticalFailure

  -- * Consumers
  , consume
  , Consumer
  , ignoreConsumer
  , pureConsumer
  , lineConsumer
  , lineLogger
  , perLine
  ) where

-- typed-process
import System.Process.Typed

-- lens
import Control.Lens hiding (argument)

-- time
import Data.Time

-- mtl
import Control.Monad.Reader

-- optparse-applicative
import Options.Applicative hiding (Success, info)

-- stm
import Control.Concurrent.STM

-- async
import Control.Concurrent.Async

-- base
import Prelude hiding (log, error)
import System.IO
import Data.IORef
import Data.Monoid
import Data.Foldable
import System.Exit
import qualified Data.List as List
import Text.Printf

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL

-- text
import qualified Data.Text.Lazy.Builder as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

import qualified Data.Text as Text

data Logger = Logger
  { _loggerHandle   :: Handle
  , _loggerPriority :: Priority
  , _loggerLevel    :: [LazyText.Text]
  , _loggerMaxDepth :: Int
  } deriving (Show, Eq)

data Priority
  = DEBUG
  | INFO
  | NOTICE
  | WARNING
  | ERROR
  | CRITICAL
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

makeClassy ''Logger

loggerDepth :: Getter Logger Int
loggerDepth = loggerLevel . to List.length

loggerInDepth :: Logger -> Ordering
loggerInDepth lg
  | lg^.loggerMaxDepth < 0 = LT
  | otherwise =
    (lg^.loggerDepth) `compare` (lg^.loggerMaxDepth)

type Builder = LazyText.Builder
type LoggerIO env m = (HasLogger env, MonadReader env m, MonadIO m)

log :: LoggerIO env m
  => Priority
  -> Builder
  -> m ()
log pri msg = do
  lg@Logger {..} <- view logger
  when (_loggerPriority <= pri && loggerInDepth lg /= GT ) $
    forM_ (LazyText.lines $ LazyText.toLazyText msg) $ \m -> do
      liftIO . LazyText.hPutStrLn _loggerHandle . LazyText.toLazyText
        $ displayf "[%7s] " pri
        <> (lg^.loggerLevel.to List.length.to (\a -> fold ["| " | _ <- [1..a]]))
        <> (lg^.loggerLevel._head.to (\a -> display a <> " | "))
        <> LazyText.fromLazyText m

debug :: LoggerIO env m => Builder -> m ()
debug = log DEBUG

info :: LoggerIO env m => Builder -> m ()
info = log INFO

notice :: LoggerIO env m => Builder -> m ()
notice = log NOTICE

warning :: LoggerIO env m => Builder -> m ()
warning = log WARNING

error :: LoggerIO env m => Builder -> m ()
error = log ERROR

critical :: LoggerIO env m => Builder -> m ()
critical = log CRITICAL

-- | Exit the program with error message
criticalFailure :: LoggerIO env m => Builder -> m a
criticalFailure msg = do
  critical msg
  liftIO $ exitFailure

timedPhase ::
  LoggerIO env m
  => LazyText.Text
  -> m a
  -> m (Double, a)
timedPhase phaseName ma = do
  lg@Logger {..} <- view logger
  case loggerInDepth lg of
    LT -> do
      local (over loggerLevel (phaseName:)) $ do
        info $ "Starting"
        (t, a) <- timeIO ma
        info $ "Done in " <> displayf " %.3fs" t
        return (t, a)
    EQ -> do
      (t, a) <- local (over loggerLevel (phaseName:)) $ timeIO ma
      info $ "Ran phase " <> display phaseName <> displayf " (%.3fs)" t
      return (t, a)
    GT ->
      timeIO ma
{-# INLINE timedPhase #-}

phase :: LoggerIO env m => LazyText.Text -> m a -> m a
phase bldr ma = snd <$> timedPhase bldr ma
{-# INLINE phase #-}

-- | Pretty printing
class Display a where
  display :: a -> Builder

displayShow :: Show a => a -> Builder
displayShow =
  displayString . show

displayf :: PrintfArg a => String -> a -> Builder
displayf fmt = LazyText.fromString . printf fmt

displayString :: String -> Builder
displayString = LazyText.fromString

displayLazyText :: LazyText.Text -> Builder
displayLazyText = LazyText.fromLazyText

displayText :: Text.Text -> Builder
displayText = LazyText.fromText

instance Display Priority where display = displayShow
instance Display Text.Text where display = displayText
instance Display LazyText.Text where display = displayLazyText

instance PrintfArg Priority where
  formatArg = formatArg . show

-- | Option parse a logger
parseLogger :: Parser Logger
parseLogger = do
  _loggerHandle <- pure stderr
  _loggerPriority <- do
    verbosity <- count $ short 'v' <> hidden <> help "make more verbose."
    quiet <- count $ short 'q' <> hidden <> help "make more quiet."
    return $
      toEnum
      . min (fromEnum CRITICAL)
      . max (fromEnum DEBUG)
      $ 2 - verbosity + quiet

  _loggerLevel <- pure []
  _loggerMaxDepth <- option auto $
    short 'D'
    <> value (-1)
    <> hidden
    <> help "set depth of logger, negative means infinite."
  return $ Logger {..}
  where count = fmap length . many . flag' ()


-- ** Utils

timeIO :: MonadIO m => m a -> m (Double, a)
timeIO m = do
  start <- liftIO getCurrentTime
  a <- m
  end <- liftIO getCurrentTime
  return (realToFrac $ diffUTCTime end start, a)

-- * Consumers

-- | Consumes the output of a command and condenses it into a sha256
consume ::
  MonadIO m
  => Consumer BS.ByteString stdout
  -> Consumer BS.ByteString stderr
  -> ProcessConfig a b c
  -> m (ExitCode, stdout, stderr)
consume outLogger errLogger cfg =
  liftIO . withProcessTerm (setStderr createPipe . setStdout createPipe $ cfg) $ \p -> do
  out <- async ( hFoldM 256 (getStdout p) outLogger )
  err <- async ( hFoldM 256 (getStderr p) errLogger )
  atomically $
    (,,) <$> waitExitCodeSTM p <*> waitSTM out <*> waitSTM err
{-# inline consume #-}

-- * Consumer

-- | A consumer, is a fold over a handle.
type Consumer b a = (a -> b -> IO a, a)

-- | Fold over a handle using a consumer
hFoldM :: Int -> Handle -> Consumer BS.ByteString a -> IO a
hFoldM size handle consumer = go (snd consumer) where
  go !acc = do
    input <- BS.hGetSome handle size
    acc' <- fst consumer acc input
    if BS.null input
      then return $! acc'
      else go acc'
{-# inline hFoldM #-}

-- | Turn a logger of lines into a LogFunc
perLine :: Consumer (Maybe BS.ByteString) a -> IO (Consumer BS.ByteString a)
perLine (consumer, i) = do
  ref <- newIORef BS.empty
  return (go ref, i)
  where
    go ref a bs
      | BS.null bs = do
          left <- readIORef ref
          if BS.null left
            then
            consumer a Nothing
            else do
            b <- consumer a (Just left)
            consumer b Nothing
      | otherwise = do
          left <- readIORef ref
          let line : restOfLines = BSC.split '\n' bs
          (a', _left) <- foldM consumeLines (a, left `BS.append` line) restOfLines
          writeIORef ref _left
          return a'
          where
            consumeLines (a', currLine) nextLine = do
              a'' <- consumer a' (Just currLine)
              return (a'', nextLine)

-- | Ignores the arguments
ignoreConsumer :: Consumer b ()
ignoreConsumer = (const . return, ())

-- | Ignores the arguments
pureConsumer :: (a -> b -> a, a) -> Consumer b a
pureConsumer (fn, initial) = (\a b -> pure $ fn a b, initial)

-- | Ignores the arguments
lineConsumer :: MonadIO m => m (Consumer BS.ByteString (Endo [LazyText.Text]))
lineConsumer = liftIO . perLine $ pureConsumer
  ( \a -> \case
      Just bs -> a <> Endo (LazyText.decodeUtf8 (BL.fromStrict bs):)
      Nothing -> a
  , Endo id
  )

-- | Ignores the arguments
lineLogger :: LoggerIO env m => Priority -> m (Consumer BS.ByteString (Endo [LazyText.Text]))
lineLogger priority = do
  _log <- ask
  liftIO $ perLine
    ( \a -> \case
        Just bs -> do
          let line = LazyText.decodeUtf8 (BL.fromStrict bs)
          runReaderT (log priority (displayLazyText line)) _log
          return $ a <> Endo (line:)
        Nothing ->
          return a
    , Endo id
    )
