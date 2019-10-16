{-# LANGUAGE TemplateHaskell #-}
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
  , Priority

  , timedPhase
  , phase


  -- * Building messages
  , Builder
  , Display (..)
  , displayShow
  , displayString

  -- * Utils
  , criticalFailure

  ) where

-- lens
import Control.Lens hiding (argument)

-- time
import Data.Time

-- mtl
import Control.Monad.Reader

-- optparse-applicative
import Options.Applicative hiding (Success, info)

-- base
import Prelude hiding (log, error)
import System.IO
import System.Exit
import qualified Data.List as List
import Text.Printf

-- text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy as Text

data Logger = Logger
  { _loggerHandle   :: Handle
  , _loggerPriority :: Priority
  , _loggerLevel    :: [Text.Text]
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

type Builder = Text.Builder
type LoggerIO env m = (HasLogger env, MonadReader env m, MonadIO m)

log :: LoggerIO env m
  => Priority
  -> Builder
  -> m ()
log pri msg = do
  lg@Logger {..} <- view logger
  when (_loggerPriority <= pri && loggerInDepth lg /= GT ) $
    forM_ (Text.lines $ Text.toLazyText msg) $ \m -> do
      liftIO . Text.hPutStrLn _loggerHandle . Text.toLazyText
        $ displayf "[%7s] " pri
        <> (lg^.loggerLevel._head.to (\a -> display a <> " | "))
        <> Text.fromLazyText m

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
  => Text.Text
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

phase :: LoggerIO env m => Text.Text -> m a -> m a
phase bldr ma = snd <$> timedPhase bldr ma
{-# INLINE phase #-}

-- | Pretty printing
class Display a where
  display :: a -> Builder

displayShow :: Show a => a -> Builder
displayShow =
  displayString . show

displayf :: PrintfArg a => String -> a -> Builder
displayf fmt = Text.fromString . printf fmt

displayString :: String -> Builder
displayString = Text.fromString

displayText :: Text.Text -> Builder
displayText = Text.fromLazyText

instance Display Priority where display = displayShow
instance Display Text.Text where display = displayText

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
