{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Nixec.Nix where

-- lens
import Control.Lens hiding ((<.>))

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- nixec
import Nixec.Rule
import Nixec.Command
import Nixec.Data

-- filepath
import System.FilePath

-- directory
import System.Directory

-- typed-process
import System.Process.Typed

-- base
import Control.Monad.IO.Class
import Data.String
import System.Exit
-- import qualified Data.List as List

-- mtl
import Control.Monad.Reader

data NixConfig = NixConfig
  { _nixFolder   :: FilePath
  , _nixOverlays :: [ FilePath ]
  }

makeClassy ''NixConfig

type HasNix env m = (MonadIO m, HasNixConfig env, MonadReader env m)

writeNixRule :: HasNix env m
  => RuleName
  -> Rule
  -> m ()
writeNixRule rn r = do
  file <- nixFile rn
  liftIO . createDirectoryIfMissing True =<< view nixFolder
  liftIO $
    writeFile file $ show (ruleToNix rn r)

nixFile :: (MonadReader env m, HasNixConfig env) => RuleName -> m FilePath
nixFile rn =
  view nixFolder <&> \folder ->
  folder </> show (ruleNameToNix rn) <.> "nix"

nixBuild :: HasNix env m
  => FilePath
  -> RuleName
  -> m ()
nixBuild output rn = do
  script <- nixBuildScript rn
  runProcess_
    . setDelegateCtlc True
    $ proc "nix-build"
      [ "-E", script
      , "-o", output
      ]

nixCheckBuild :: HasNix env m => RuleName -> m (Maybe FilePath)
nixCheckBuild rn = do
  output <- dropExtension <$> nixFile rn
  script <- nixBuildScript rn
  (exit,_,_) <- readProcess
    . setDelegateCtlc True
    $ proc "nix-build"
      [ "-E", script
      , "-o", output
      , "--readonly-mode"
      ]
  case exit of
    ExitSuccess ->
      return $ Just output
    _ -> do
      return $ Nothing

nixBuildScript ::
  HasNix env m
  => RuleName
  -> m String
nixBuildScript rn = do
  file <- nixFile rn
  fps <- view nixOverlays
  overlays <- fmap concat . forM fps $ \fp -> do
    b <- liftIO $ doesFileExist fp
    return [ "(import ./" ++ fp ++ ")" | b ]

  return . show $
    "((import <nixpkgs> {}).appendOverlays ["
    <> vsep (map pretty overlays) <>
    "]).callPackage"
    <+> "./" <> fromString file
    <+> "{}"
  -- return . show $
  --   "(import <nixpkgs> { overlays = ["
  --   <> vsep (map pretty overlays)
  --   <> "]; }).callPackage"
  --   <+> "./" <> fromString file
  --   <+> "{}"

nixBuildAll :: (HasNix env m, Foldable f)
  => f RuleName
  -> m ()
nixBuildAll rns = do
  forM_ rns $ \rn -> do
    output <- dropExtension <$> nixFile rn
    nixBuild output rn








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
      [ pretty (superPackage p)
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
          [ "${time}/bin/time", "--format"
          , dquotes ((pretty rn) <> ",%e,%U,%S,%M,%x")
          , "--append", "--output", "times.csv"
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

nixstring :: Doc m -> Doc m
nixstring = enclose
  (flatAlt "\"" ("''" <> hardline))
  (flatAlt "\"" (hardline <> "''"))
