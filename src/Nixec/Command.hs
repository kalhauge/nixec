{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Nixec.Command where

-- base
import Data.String
import Data.Data

-- lens
import Control.Lens

-- text
import qualified Data.Text as Text

import Nixec.Data

data Command = Command
  { _commandProgram :: !Text.Text
  , _commandArgs    :: ![ CommandArgument ]
  , _commandStderr  :: !(Maybe FilePath)
  , _commandStdout  :: !(Maybe FilePath)
  }

makeCommand :: Text.Text -> Command
makeCommand t =
  Command t [] Nothing Nothing

-- A Command Argument
data CommandArgument
  = Input      !FilePath
  -- ^ An command input
  | Output      !FilePath
  -- ^ A command output
  | Global      !Input
  -- ^ A global input
  | RegularArg !Text.Text
  -- ^ A simple string
  | ConcatArg  Text.Text [ CommandArgument ]
  -- ^ Concatenate the other arguments
  deriving (Show, Eq, Ord, Data)

instance IsString CommandArgument where
  fromString = RegularArg . Text.pack

(<.+>) :: CommandArgument -> CommandArgument -> CommandArgument
cm1 <.+> cm2 = ConcatArg "" [cm1, cm2]

makeClassy ''Command
makePrisms ''CommandArgument

instance Plated CommandArgument
