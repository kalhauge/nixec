{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Nixec where

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

untar :: Input -> NixecM RuleName
untar i =
  rule "untar"
  . cmd "tar"
  $ commandArgs .= [ "-xf", Global i]


example :: IO ()
example = defaultMain $ do
  let predicates = FileInput "predicates"
  benchmarks <- untar (FileInput "benchmarks.tar.gz")

  let benchmarkNames = [ "mybenchmark" ]
  benchs <- forM benchmarkNames $ \name -> scope name $ do

    let benchmark = benchmarks <./> toFilePath name

    let predicateNames = [ "cfr" , "fernflower"] -- , "procyon" ]
    runs <- forM predicateNames $ \predicate -> scope predicate $ do
      run <- rule "run" $ do
        needs
          [ "benchmark" ~> benchmark
          , "predicate" ~> predicates <./> toFilePath predicate
          ]
        cmd "predicate" $ commandArgs .=
          [ Input "benchmark/classes" , Input "benchmark/lib" ]

      reduce <- onSuccess run $ do
        let strategies = [ "classes" , "methods"] -- , "interfaces" ]

        reductions <- forM strategies $ \strategy -> rule strategy $ do
            needs
              [ "benchmark" ~> benchmark
              , "predicate" ~> predicates <./> toFilePath predicate
              ]
            path [ "haskellPackages.jreduce" ]
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
              , "%" <.+> Input "benchmarks/lib"
              ]

        rule "reduce" $ do
          needs [ toFilePath (topRuleName c) ~> c | c <- reductions ]
          cmd "extract.py" $ commandArgs .=
            [ Input $ toFilePath (topRuleName c) | c <- reductions ]
          exists "result.csv"

      collect $ do
        needs [ "run" ~> run ]
        joinCsv fields (maybeToList reduce) "result.csv"

    collect $ do
      joinCsv fields runs "result.csv"

  collect $ do
    joinCsv fields benchs "result.csv"

  where
    fields = [ "benchmark", "predicate", "strategy" ]
