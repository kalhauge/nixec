{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Nixec.Config where

-- base
import Prelude hiding (log)

-- text
import qualified Data.Text as Text

-- lens
import Control.Lens hiding ((<.>))

-- optparse-applicative
import Options.Applicative hiding (Success)

-- nixec
import Nixec.Data
import Nixec.Nix

data Action
  = RunAction

data Config = Config
  { _configScope     :: Scope
  , _configNixConfig :: NixConfig
  , _configTarget    :: Maybe RuleName
  , _configDryRun    :: Bool
  , _configCheck     :: Bool
  , _configAction    :: Action
  }

-- defaultConfig :: Config
-- defaultConfig = Config
--   []
--   (NixConfig "_nixec/nix" [ "overlay.nix" ] True "mkRule" ("nix-build", []))
--   Nothing
--   $ RunAction

makeClassy ''Config

parseConfig :: Parser Config
parseConfig = do
  _configScope <- pure []

  _configNixConfig <- do
    _nixOverlays <-
      fmap ("overlay.nix":) . many . strOption
      $ short 'O'
      <> long "overlays"
      <> metavar "FILE"
      <> help "Nix overlays (default: 'overlay.nix')"
      <> hidden

    _nixFolder <- pure "_nixex/nix"

    _nixBuildCommand <- pure ("nix-build", [])

    _nixMkRule <- pure "mkRule"

    _nixVerbose <- switch $
      short 'V'
      <> help "show the output of running the nixscripts."

    pure $ NixConfig {..}

  _configAction <- pure $ RunAction
    -- hsubparser . fold $
    -- [ command "list" $ info (pure ListAction)
    --   (progDesc "Print list of rules")
    -- , command "run" $ info
    --   (ExecAction .
    --   (progDesc "run derivations")
    -- ]
  _configDryRun <- switch $
    long "dry"
    <> help "don't actually exeucute any nix-scripts."

  _configCheck <- switch $
    long "check"
    <> help "check that the database is correct."

  _configTarget <-
    optional
    . fmap (ruleNameFromText . Text.pack)
    . strArgument
    $ metavar "RULE"
      <> help "the rule to run."

  pure $ Config {..}

instance HasNixConfig Config where
  nixConfig = configNixConfig
