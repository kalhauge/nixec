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


  -- * Building messages
  , Builder
  , Display (..)
  , displayShow
  , displayString

  -- * Utils
  , criticalFailure

  ) where


-- lens
import Control.Lens

-- mtl
import Control.Monad.Reader

-- optparse-applicative
import Options.Applicative hiding (Success, info)

-- base
import Prelude hiding (log, error)
import System.IO
import System.Exit

-- text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy as Text

data Logger = Logger
  { _loggerHandle :: Handle
  , _loggerLevel :: Priority
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

type Builder = Text.Builder
type LoggerIO env m = (HasLogger env, MonadReader env m, MonadIO m)

log :: (HasLogger env, MonadReader env m, MonadIO m)
  => Priority
  -> Builder
  -> m ()
log pri msg = do
  Logger {..} <- view logger

  when (_loggerLevel <= pri) $
    forM_ (Text.lines $ Text.toLazyText msg) $ \m -> do
      liftIO . Text.hPutStrLn _loggerHandle $
        Text.toLazyText ("(" <> display pri <> ") " <> Text.fromLazyText m)


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

-- | Pretty printing
class Display a where
  display :: a -> Builder

displayShow :: Show a => a -> Builder
displayShow =
  displayString . show

displayString :: String -> Builder
displayString =
  Text.fromString

instance Display Priority where display = displayShow

parseLogger :: Parser Logger
parseLogger = do
  _loggerHandle <- pure stderr
  _loggerLevel <- do
    verbosity <- count $ short 'v' <> hidden <> help "make more verbose."
    quiet <- count $ short 'q' <> hidden <> help "make more quiet."
    return $
      toEnum
      . min (fromEnum CRITICAL)
      . max (fromEnum DEBUG)
      $ 2 - verbosity + quiet

  return $ Logger {..}
  where count = fmap length . many . flag' ()
