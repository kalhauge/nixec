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
  { _program :: !CommandArgument
  , _args    :: ![ CommandArgument ]
  , _stderr  :: !(Maybe FilePath)
  , _stdout  :: !(Maybe FilePath)
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
  | DebugArgs
  -- ^ Add a location for debug arguments $@
  deriving (Show, Eq, Ord, Data)

argViaShow :: Show a => a -> CommandArgument
argViaShow = RegularArg . Text.pack . show

instance IsString CommandArgument where
  fromString = RegularArg . Text.pack

(<.+>) :: CommandArgument -> CommandArgument -> CommandArgument
cm1 <.+> cm2 = ConcatArg "" [cm1, cm2]

makeClassy ''Command
makePrisms ''CommandArgument

renderCommands :: [Command] -> Doc m
renderCommands commands = vsep . concat $
  [ [ "SCRIPTPATH=\"$( cd \"$(dirname \"$0\")\" ; pwd -P )\""
    , "WORKDIR=${WORKDIR:-\"$(pwd)\"}"
    , "INPUTDIR=${INPUTDIR:-$SCRIPTPATH}"
    ]
  , [ splitcommand $ concat
      [ [ commandArgToShell (c^.program) ]
      , [ commandArgToShell ca | ca <- c^. args]
      , [ ">>" <> (dquotes . pretty $ "$WORKDIR" </> fp)
        | fp <- maybeToList (c ^. stdout)]
      , if c ^. stderr == c ^. stdout && c ^. stderr /= Nothing
        then [ "2>&1" ]
        else [ "2>>" <> (dquotes . pretty $ "$WORKDIR" </> fp)
             | fp <- maybeToList (c ^. stderr)]
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
      ConcatArg t _args ->
        concatWith (\a b -> a <> pretty t <> b) . map commandArgToShell $ _args
      DebugArgs ->
        "$@"

splitcommand :: [Doc m] -> Doc m
splitcommand =
  nest 2 . concatWith (\a b -> a <> group linesep <> b)
  where linesep = (flatAlt (" \\" <> line) space)
  -- group  <> group b)
