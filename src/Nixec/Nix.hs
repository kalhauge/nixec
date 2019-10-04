{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- text
import qualified Data.Text as Text

import Data.Text.Encoding

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
  { _nixFolder       :: FilePath
  , _nixOverlays     :: [ FilePath ]
  , _nixVerbose      :: Bool
  , _nixMkRule       :: Package
  , _nixBuildCommand :: (String, [ String ])
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
  script <- ruleToNix rn r
  liftIO . writeFile file $ show script

nixFile :: (MonadReader env m, HasNixConfig env) => RuleName -> m FilePath
nixFile rn =
  view nixFolder <&> \folder ->
  folder </> show (ruleNameToNix rn) <.> "nix"

nixBuild ::
  HasNix env m
  => String
  -> m (Maybe FilePath)
nixBuild script = do
  (_cmd, _args) <- view nixBuildCommand
  let a = setDelegateCtlc True $ proc _cmd (_args ++ [ "-E", script ])
  verbose <- view nixVerbose
  (exit, s) <- if verbose
    then readProcessStdout a
    else do
    (exit, s, _) <- readProcess a
    return (exit, s)
  case exit of
    ExitSuccess ->
      return $ Just (Text.unpack . Text.strip . decodeUtf8 . BL.toStrict $ s)
    _ -> do
      return $ Nothing

withArgs :: HasNix env m => [String] -> m a -> m a
withArgs _args =
  local (nixBuildCommand._2 %~ (++ _args))

nixCheckBuildInput :: HasNix env m => Input -> m (Maybe FilePath)
nixCheckBuildInput =
  local (nixVerbose .~ False)
  . withArgs ["--readonly-mode"]
  . nixBuildInput

nixBuildInput :: HasNix env m => Input -> m (Maybe FilePath)
nixBuildInput = \case
  PackageInput p -> do
    withArgs ["--no-out-link"] $
      nixBuild =<< nixPackageScript p
  RuleInput rn -> do
    output <- dropExtension <$> nixFile rn
    withArgs ["-o", output] $
      nixBuild =<< nixRuleScript rn
  FileInput fp -> do
    return (Just fp)
  InInput i ex ->
   fmap (</> ex) <$> nixBuildInput i

nixRuleScript :: HasNix env m => RuleName -> m String
nixRuleScript rn = do
  file <- nixFile rn
  nixPackageScript' ("callPackage" <+> "./" <> fromString file <+> "{}")

nixPackageScript :: HasNix env m => Package -> m String
nixPackageScript = nixPackageScript' . pretty

nixPackageScript' ::
  HasNix env m
  => Doc x
  -> m String
nixPackageScript' pkg' = do
  fps <- view nixOverlays
  overlays <- fmap concat . forM fps $ \fp -> do
    b <- liftIO $ doesFileExist fp
    return [ "(import ./" ++ fp ++ ")" | b ]

  return . show $
    "((import <nixpkgs> {}).appendOverlays ["
    <> vsep (map pretty overlays) <>
    "])." <> pkg'

nixBuildAll :: (HasNix env m, Foldable f)
  => f Input
  -> m ()
nixBuildAll rns = do
  forM_ rns $ nixBuildInput

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

rulesToNix :: (HasNixConfig env, MonadReader env m)
  => [(RuleName, Rule)]
  -> m (Doc e)
rulesToNix rules = do
  rs <- mapM (\r -> (ruleNameToNix (r^._1),) <$> uncurry ruleToNix r) rules
  return $ "{ nixpkgs ? import <nixpkgs> {} }: with nixpkgs;" <+> attrset rs


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

ruleToNix ::
  (HasNixConfig env, MonadReader env m) => RuleName -> Rule -> m (Doc e)
ruleToNix rn r = do
  header <- mkHeader <$> view nixMkRule
  scrpt <- ruleToNixDrv rn r
  return $ header <> ":" <> line <> scrpt
  where
    mkHeader mkRule = encloseSep "{ " " }" ", " $
      ["stdenv", "callPackage", "time"]
      ++
      [ pretty (superPackage p)
      | p <- mkRule:toListOf (folding ruleInputs.cosmosOf (_InInput._1)._PackageInput) r
      ]

ruleToNixDrv ::
  (HasNixConfig env, MonadReader env m) => RuleName -> Rule -> m (Doc e)
ruleToNixDrv rn r = do
  mkRule <- view nixMkRule
  return $
    pretty mkRule <+>
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
            , "sed -i -e '/Command/d' 'times.csv'"
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
