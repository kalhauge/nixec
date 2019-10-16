{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Nixec
  ( module Nixec.Data
  , module Nixec.Builder
  , module Nixec.Rule
  , module Nixec.Command
  , module Nixec.Monad
  , module Control.Lens
  , example
  ) where

-- base
import Prelude hiding (log)

-- lens
import Control.Lens

-- text
import qualified Data.Text as Text

-- nixec
import Nixec.Data
import Nixec.Rule hiding (rule)
import Nixec.Command
import Nixec.Monad
import Nixec.Builder

example :: IO ()
example = defaultMain $ do
  benchmarks <- listFiles benchmarkpkg $ Text.stripSuffix "_tgz-pJ8" . Text.pack

  collectWith resultCollector . scopesBy fst benchmarks $ \(_, benchmark) ->
    collectWith resultCollector . scopes predicateNames $ \predicate -> do
      run <- rule "run" $ do
        benc <- link "benchmark" benchmark
        predi <- link "prediate" $ predicates <./> toFilePath predicate
        cmd predi $ args .= [ benc <.+> "/classes" , benc <.+> "/lib"]

      reduce <- onSuccess run $ do
        reductions <- rules strategies $ \strategy -> do
          benc <- link "benchmark" benchmark
          predi <- link "prediate" $ predicates <./> toFilePath predicate
          path [ "haskellPackages.jreduce" ] --, "openjdk", "unzip"]
          cmd "jreduce" $ args .=
            [ "-W", Output "workfolder"
            , "-p", "out,exit"
            , "--total-time", "3600"
            , "--strategy", RegularArg strategy
            , "--output-file", Output "reduced"
            , "--stdlib", "--cp", benc <.+> "/lib"
            , benc <.+> "/classes"
            , predi , "{}" , "%" <.+> benc <.+> "/lib"
            ]

        rule "reduce" $ do
          rs <- asLinks reductions
          extract <- link "extract.py" (FileInput "bin/extract.py")
          path [ "python3" ]
          cmd "python3" $ args .= extract:rs
          exists "result.csv"

      collect $ do
        needs [ "run" ~> run ]
        r <- asLinks reduce
        joinCsv resultFields r "result.csv"

  where
    predicateNames =
      [ "cfr" , "fernflower", "procyon"]

    strategies =
      [ "class" , "methods", "interfaces" ]

    benchmarkpkg =
      PackageInput "benchmarks"

    predicates =
      FileInput "predicate"

    resultFields =
      [ "benchmark", "predicate", "strategy" ]

    resultCollector x =
      joinCsv resultFields x "result.csv"
