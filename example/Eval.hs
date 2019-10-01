{-# LANGUAGE OverloadedStrings #-}

import Nixec

main :: IO ()
main = defaultMain $ do
  benchs <- forM benchmarks $ \name -> scope name $ do
    benchmark <- untar (FileInput "benchmarks.tar.gz") name
    predicates <- load (FileInput "predicates")

    let predicates =
          [ "cfr"
          , "fernflower"
          , "procyon"
          ]

    runs <- forM predicates $ \predicate -> scope predicate $ do
      run <- rule "run-predicate" $ do
        needs
          [ "benchmark" ~> benchmark
          , "predicate" ~> predicates <./> predicate
          ]
        cmd "predicate" 
          [ Input "benchmark/classes"
          , Input "benchmark/lib"
          ]

      result <- onSuccess run $ do
        let strategies = [ "classes" , "methods" , "interfaces" ]

        computations <- forM strategies $ \strategy -> scope strategy $ do
          rule "compute" $ do
            needs
              [ "benchmark" ~> benchmark
              , "predicate" ~> predicates <./> predicate
              ]
            path [ pkgs "haskellPackages.jreduce" ]
            cmd "jreduce"
              [ "-W", Output "workfolder"
              , "-p", "out,exit"
              , "--total-time", "3600"
              , "--strategy", strategy
              , "--output-file", Output "reduced"
              , "--stdlib"
              , "--cp", Input "benchmark/lib"
              , Input "benchmark/classes"
              , Input "predicate", "{}"
              , "%" <.+> Input "benchmarks/lib"
              ]

        rule "post" $ do
          needs [ ruleName c ~> c | c <- computations ]
          cmd "extract.py" [ File (ruleName c) | c <- computations ]
          exists "result.csv"

      join [run, result]


    rule "post" $ do
      joinCsv runs "result.csv"

  rule "post" $ do
    joinCsv runs "result.csv"
