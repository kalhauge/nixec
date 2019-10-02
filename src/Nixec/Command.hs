{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Nixec.Command where

-- base
import Data.String
import Data.Data
import Data.Maybe

-- lens
import Control.Lens

-- filesystem
import System.FilePath

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- text
import qualified Data.Text as Text

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
  -- ^ A command input
  | Output      !FilePath
  -- ^ A command output
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

renderCommands :: [Command] -> Doc m
renderCommands commands = vsep . concat $
  [ [ "WORKDIR=''${1:-\"$(pwd)\"}"
    , "INPUTDIR=''${2:-$WORKDIR}"
    ]
  , [ hsep $ concat
      [ [ commandArgToShell (c^.commandProgram) ]
      , [ commandArgToShell ca | ca <- c^. commandArgs]
      , [ ">>" <> (dquotes . pretty $ "$WORKDIR" </> fp)
        | fp <- maybeToList (c ^. commandStdout)]
      , if c ^. commandStderr == c ^. commandStdout && c ^. commandStderr /= Nothing
        then [ "2>&1" ]
        else [ "2>>" <> (dquotes . pretty $ "$WORKDIR" </> fp)
             | fp <- maybeToList (c ^. commandStderr)]
      ]
    | c <- commands
    ]
  ]
  where
    commandArgToShell = \case
      Input fp -> dquotes . pretty $ "$INPUTDIR" </> fp
      Output fp -> dquotes . pretty $ "$WORKDIR" </> fp
      RegularArg i
        | Text.any (\c -> c == ';' || c == ' ') i -> dquotes (pretty i)
        | otherwise -> pretty i
      ConcatArg t args ->
        concatWith (\a b -> a <> pretty t <> b) . map commandArgToShell $ args
