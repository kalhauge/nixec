{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Nixec
  ( module Nixec.Data
  , module Nixec.Rule
  , module Nixec.Command
  , module Nixec.Monad
  , module Control.Lens
  , example
  ) where

-- base
import Prelude hiding (log)
import Data.Maybe

-- lens
import Control.Lens

-- directory
-- import System.Directory

-- mtl
import Control.Monad.State

-- prettyprinter
-- import Data.Text.Prettyprint.Doc

-- Nixec
import Nixec.Data
import Nixec.Rule hiding (rule)
import Nixec.Command
import Nixec.Monad

example :: IO ()
example = mainWithConfig
  ( defaultConfig
    & configAction .~ -- GetRulesAction
    ExecAction (ruleNameFromText "urlfc5806b04b_wlu_mstr_leveldb_java")
  ) $ do
  let predicates = FileInput "example/predicate"
  let benchmarks = PackageInput "benchmarks"
  let env =
        [ Env "CFR"
          (FileInput "example/decompilers/cfr/cfr_0_132.jar" )
        , Env "PROCYON"
          (FileInput "example/decompilers/procyon/procyon-decompiler-0.5.30.jar" )
        , Env "FERNFLOWER"
          (FileInput "example/decompilers/fernflower/fernflower.jar" )
        ]

  let benchmarkNames = [ "urlfc5806b04b_wlu_mstr_leveldb_java" ]
  benchs <- forM benchmarkNames $ \name -> scope name $ do

    let benchmark = benchmarks <./> (toFilePath name ++ "_tgz-pJ8")

    let predicateNames = [ "cfr" , "fernflower"] -- , "procyon" ]
    runs <- forM predicateNames $ \predicate -> scope predicate $ do
      run <- rule "run" $ do
        needs
          [ "benchmark" ~> benchmark
          , "predicate" ~> predicates <./> toFilePath predicate
          ]
        needs env
        path [ "openjdk", "unzip", "time", "coreutils"]
        cmd (Input "predicate") $ commandArgs .=
          [ Input "benchmark/classes" , Input "benchmark/lib" ]

      reduce <- onSuccess run $ do
        let strategies = [ "class" , "methods"] -- , "interfaces" ]

        reductions <- forM strategies $ \strategy -> rule strategy $ do
            needs
              [ "benchmark" ~> benchmark
              , "predicate" ~> predicates <./> toFilePath predicate
              ]
            needs env
            path [ "haskellPackages.jreduce", "openjdk", "unzip", "time", "coreutils"]
            cmd "jreduce" $ commandArgs .=
              [ "-W", Output "workfolder"
              , "-p", "out,exit"
              , "--total-time", "3600"
              , "--strategy", RegularArg strategy
              , "--output-file", Output "reduced"
              , "--stdlib"
              , "--cp", Input "benchmark/lib"
              , Input "benchmark/classes"
              , Input "predicate", "{}"
              , "%" <.+> Input "benchmark/lib"
              ]

        rule "reduce" $ do
          needs [ toFilePath (topRuleName c) ~> c | c <- reductions ]
          needs [ "extract.py" ~> FileInput "extract.py"]
          path [ "python3", "time", "coreutils" ]
          cmd "python3" $ do
            commandArgs .= concat
              [ [ Input "extract.py" ]
              , [ RegularArg name, RegularArg predicate ]
              , [ Input $ toFilePath (topRuleName c) | c <- reductions ]
              ]
            commandStdout .= Just "result.csv"
          exists "result.csv"

      collect $ do
        needs [ "run" ~> run]
        path [ "time", "coreutils" ]
        joinCsv fields (maybeToList reduce) "result.csv"

    collect $ do
      path [ "time", "coreutils" ]
      joinCsv fields runs "result.csv"

  collect $ do
    path [ "time", "coreutils" ]
    joinCsv fields benchs "result.csv"

  where
    fields = [ "benchmark", "predicate", "strategy" ]
