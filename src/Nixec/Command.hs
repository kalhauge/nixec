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
  { _commandProgram :: !CommandArgument
  , _commandArgs    :: ![ CommandArgument ]
  , _commandStderr  :: !(Maybe FilePath)
  , _commandStdout  :: !(Maybe FilePath)
  }

makeCommand :: CommandArgument -> Command
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

commandArgumentInputs :: Fold CommandArgument Input
commandArgumentInputs =
  cosmosOf (_ConcatArg._2.folded)._Global

commandInputs ::
  (Semigroup (f Command), Contravariant f, Applicative f)
  => (Input -> f Input)
  -> Command
  -> f Command
commandInputs =
  commandProgram.commandArgumentInputs
  <> commandArgs.folded.commandArgumentInputs
