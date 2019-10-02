{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixec.Nix where

-- base
import Data.Maybe

-- lens
import Control.Lens

-- filepath
import System.FilePath

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- text
import qualified Data.Text as Text

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

renderCommands :: [Command] -> [Doc m]
renderCommands commands =
  [ "WORKDIR=''${1:-\"$(pwd)\"}"
  , "INPUTDIR=''${1:-$WORKDIR}"
  ] ++
  [ hsep $ concat
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
  where
    commandArgToShell = \case
      Input fp -> dquotes . pretty $ "$INPUTDIR" </> fp
      Output fp -> dquotes . pretty $ "$WORKDIR" </> fp
      Global i -> inputToShell i
      RegularArg i
        | Text.any (\c -> c == ';' || c == ' ') i -> dquotes (pretty i)
        | otherwise -> pretty i
      ConcatArg t args ->
        concatWith (\a b -> a <> pretty t <> b) . map commandArgToShell $ args

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
    , dquotes (ruleNameToNix rn)
    )
  , ( "buildInputs"
    , encloseSep "[" "]" " "
      [ pretty p
      | OnPath p <- r ^. ruleRequires
      ]
    )
  , ( "command"
    , "builtins.toFile" <+> dquotes "run.sh" <+> script (renderCommands (r ^. ruleCommands))
    )
  , ( "unpackPhase"
    , ( script . concat $
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
    , script [ "sh run.sh 2>&1 >>output.log"]
    )
  , ( "installPhase"
    , script
      [ "shopt -s dotglob"
      , "mkdir -p $out"
      , "mv * $out"
      ]
    )
  ]

  where
    script a = line <>
      indent 2 (
        enclose ("''" <> softline) (softline <> "''")
        . align
        $ vsep a
        )
