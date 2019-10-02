{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixec.Nix where

-- lens
import Control.Lens

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- nixec
import Nixec.Rule
import Nixec.Command
import Nixec.Data

ruleNameToNix :: RuleName -> Doc m
ruleNameToNix =
  foldr1 (\a b -> b <> "-" <> a) . fmap pretty . unRuleName

ruleNameToCallPackage :: RuleName -> Doc m
ruleNameToCallPackage rn =
  "callPackage" <+> "./" <> ruleNameToNix rn <> ".nix" <+> "{}"


nixToShell :: Doc m -> Doc m
nixToShell = enclose "${" "}"

inputToShell :: Input -> Doc m
inputToShell = \case
  RuleInput r -> nixToShell (ruleNameToCallPackage r)
  PackageInput r -> nixToShell (pretty r)
  FileInput r -> nixToShell ("../../" <> pretty r)
  InInput r fp -> inputToShell r <> "/" <> pretty fp

rulesToNix :: [(RuleName, Rule)] -> Doc m
rulesToNix rules =
  "{ nixpkgs ? import <nixpkgs> {} }: with nixpkgs;" <+>
  ( attrset
    . map (\r -> (ruleNameToNix (r^._1), uncurry ruleToNix r))
    $ rules
  )

attrset :: [(Doc m, Doc m)] -> Doc m
attrset =
  braces
  . enclose line line
  . indent 2
  . vsep
  . map (\(n, m) -> n <+> "=" <+> m <> ";")

letlist :: [(Doc m, Doc m)] -> Doc m -> Doc m
letlist lst inexp =
  "let"
  <+> ( enclose softline softline
       . indent 2
       . vsep
       . map (\(n, m) -> n <+> "=" <+> m <> ";")
       $ lst
     )
  <+> "in"
  <+> inexp

ruleToNix :: RuleName -> Rule -> Doc m
ruleToNix rn r =
  header <> ":" <> line <> ruleToNixDrv rn r

  where
    header = encloseSep "{ " " }" ", " $
      ["stdenv", "callPackage"]
      ++
      [ pretty (p :: Package)
      | p <- toListOf (folding ruleInputs.cosmosOf (_InInput._1)._PackageInput) r
      ]


ruleToNixDrv :: RuleName -> Rule -> Doc m
ruleToNixDrv rn r =
  ("stdenv.mkDerivation" <+>) $
  attrset
  [ ( "name"
    , nixstring (ruleNameToNix rn)
    )
  , ( "buildInputs"
    , encloseSep "[" "]" " "
      [ pretty p
      | OnPath p <- r ^. ruleRequires
      ]
    )
  , ( "command"
    , "builtins.toFile" <+> dquotes "run.sh" <+>
      script (renderCommands (r ^. ruleCommands))
    )
  , ( "unpackPhase"
    , ( script . vcat . concat $
        [ [ "ln -s" <+> inputToShell i <+> squotes (pretty fp)
          | LinkTo fp i <- r ^. ruleRequires
          ]
        , [ "ln -s $command run.sh"
          , "chmod +x run.sh"
          ]
        ]
      )
    )
  , ( "buildPhase"
    , script . vcat . concat $
      [ [ "export " <> pretty n <> "=" <> dquotes (inputToShell i)
        | Env n i <- r ^. ruleRequires
        ]
      -- , [ "timeout" <+> pretty timelimit ]
      , [ "echo" <+> dquotes "rule,real,user,kernel,maxm,exitcode" <+> ">times.csv"
        , splitcommand
          [ "${time}/bin/time", "--format", dquotes "$name,%e,%U,%S,%M,%x", "--output", "times.csv"
          , "sh", "run.sh", "1>", ">(tee stdout)", "2>", ">(tee stderr >&2)"
          , "||:"]
        ]
      ]
    )
  , ( "installPhase"
    , script . vcat $
      [ "mkdir -p $out"
      , "mv * $out"
      ]
    )
  ]

  where
    script :: Doc m -> Doc m
    script a = vcat $ [ "''", indent 2 a , "''"]

splitcommand :: [Doc m] -> Doc m
splitcommand =
  nest 2 . concatWith (\a b -> a <> group linesep <> b)
  where linesep = (flatAlt (" \\" <> line) space)
  -- group  <> group b)


nixstring :: Doc m -> Doc m
nixstring = enclose
  (flatAlt "\"" ("''" <> hardline))
  (flatAlt "\"" (hardline <> "''"))
