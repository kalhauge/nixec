{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Nixec.Nix
  (
  -- * Configure
    NixConfig (..)
  , HasNixConfig (..)
  , withArgs
  , HasNix

  -- * Configure Nix-Build
  , nixBuild
  , nixBuildWithPkgsAndOverlays
  , nixBuildRules

  -- * Write rule
  , writeRule
  , writeDatabase
 
  -- * Expressions
  , databaseExpr
  , ruleExpr

  , withPkgsAndOverlaysExpr
  , callFileExpr
  , callPackageExpr

  -- * Handle RuleName to Files
  , ruleNameToFilePath
  ) where

-- lens
import Control.Lens hiding ((<.>))

-- containers
import qualified Data.Set as Set

-- data-fix
import Data.Fix

-- filepath
import System.FilePath

-- hnix
import Nix

-- cassava
import qualified Data.Csv as Csv

-- directory
import System.Directory

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Encoding as Text

-- typed-process
import System.Process.Typed

-- base
import Control.Monad.IO.Class
import Data.Monoid
import System.Exit
import qualified Data.List as List

-- mtl
import Control.Monad.Reader

-- nixec
import Nixec.Rule
import Nixec.Command
import qualified Nixec.Logger as L


data NixConfig = NixConfig
  { _nixOverlays     :: [ FilePath ]
  , _nixBuildCommand :: (String, [ String ])
  } deriving (Show, Eq)

makeClassy ''NixConfig


type HasNix env m = (MonadIO m, HasNixConfig env, MonadReader env m)

withArgs :: HasNix env m => [String] -> m a -> m a
withArgs _args =
  local (nixBuildCommand._2 %~ (++ _args))

-- | Call Nix-Build with an NExpr
nixBuild ::
  (HasNix env m, L.HasLogger env)
  => NExpr
  -> m (Maybe FilePath)
nixBuild expr = L.phase "nix" $ do
  (_cmd, _args) <- view nixBuildCommand

  let script = show (prettyNix expr)

  let a = proc _cmd (_args ++ [ "-E", script ])
  L.info $ "Building script: "
  L.info $ L.displayString script

  _stdout <- L.lineConsumer
  _stderr <- L.lineLogger L.DEBUG

  (exit, out, err) <- L.consume _stdout _stderr a

  case exit of
    ExitSuccess ->
      return $ Just (LazyText.unpack . LazyText.strip . LazyText.unlines $ appEndo out [])
    e -> do
      L.warning $ "Build failed with " <> L.displayShow e
      let _list = appEndo err []
      forM_ (drop (List.length _list - 10) _list) $ L.info . L.displayLazyText
      return $ Nothing

-- | Convient wrapper around nixBuild and withPkgsAndOverlaysExpr
nixBuildWithPkgsAndOverlays ::
  (L.HasLogger env, HasNix env m, AsExpr a)
  => a
  -> m (Maybe FilePath)
nixBuildWithPkgsAndOverlays a =
  nixBuild =<< withPkgsAndOverlaysExpr a


-- | Run the expression in a environment where all packages and overlays have
-- been loaded
withPkgsAndOverlaysExpr ::
  (HasNix env m, AsExpr a) => a -> m NExpr
withPkgsAndOverlaysExpr (toExpr -> expr) = do
  fps <- view nixOverlays

  overlays <- fmap concat . forM fps $ \fp -> do
   liftIO (doesFileExist fp) <&> \b ->
     [ mkSym "import" @@ mkPath False fp | b ]

  let nixpkgs = mkSym "import" @@ mkEnvPath "nixpkgs" Nix.@@ attrsE []
  return $ mkWith (nixpkgs @. "appendOverlays" @@ mkList overlays) expr

nixBuildRules ::
  (HasNix env m, L.HasLogger env)
  => FilePath
  -> [RuleName]
  -> m (Maybe FilePath)
nixBuildRules folder rules =
  nixBuildWithPkgsAndOverlays
    [ callFileExpr (ruleNameToFilePath t folder) []
    | t <- rules
    ]

-- | 'callPackage <file> { .. }'
callFileExpr :: FilePath -> [ (Text.Text, NExpr) ] -> NExpr
callFileExpr = callPackageExpr . mkPath False

-- | 'callPackage <expr> { .. }'
callPackageExpr :: NExpr -> [ (Text.Text, NExpr) ] -> NExpr
callPackageExpr e b = mkSym "callPackage" @@ e @@ attrsE b

-- | Write a rule to a folder
writeRule :: MonadIO m => FilePath -> Package -> RuleName -> Rule -> m ()
writeRule folder mkRule rn r = liftIO $ do
  let fn = ruleNameToFilePath rn folder
  createDirectoryIfMissing True (takeDirectory fn)
  writeFile fn . show . prettyNix $ ruleExpr mkRule rn r

-- | Write a rule to a folder
writeDatabase :: MonadIO m => FilePath -> Set.Set InputFile -> m ()
writeDatabase fn missing = liftIO $ do
  writeFile fn . show . prettyNix $ databaseExpr missing

ruleExpr :: Package -> RuleName -> Rule -> NExpr
ruleExpr mkRule rn r = mkFunction header (toExpr mkRule @@ body) where
  header = flip mkParamset False . map (,Nothing) . concat $
    [ [ "stdenv"
      , "callPackage"
      , "time"
      ]
    , [ packageToText $ superPackage p
      | p <- mkRule : toListOf (folding ruleInputFiles.inputFileInput._PackageInput) r
      ]
    ]

  body = attrsE
    [ ( "name", mkStr (topRuleName rn))
    , ( "buildInputs", toExpr [ p | OnPath p <- r ^. ruleRequires])
    , ( "command", mkSym "builtins.toFile"
        @@ mkStr "run.sh"
        @@ mkIndentedStr 2 (Text.pack . show $ renderCommands (r ^. ruleCommands))
      )
    , ( "unpackPhase", Fix . Nix.NStr . Nix.Indented 2 . List.intercalate [Plain "\n"] $
        [ [ Plain "# This is a comment to make sure that the output is put on multiple lines" ]
        , List.intercalate [Plain "\n"]
          [ [ Plain "ln -s "]
            ++ inputFileToNixString i
            ++ [ Plain (Text.pack $ ' ': fp) ]
          | LinkTo fp i <- r ^. ruleRequires
          ]
        , [ Plain "ln -s $command run.sh" ]
        ]
      )
    , ( "rulename", mkStr (ruleNameToText rn) )
    , ( "time", mkSym "time" )
    , ( "buildPhase",
        mkIndentedStr 2 . Text.pack . show . vcat $
        [ "echo" <+> dquotes "rule,real,user,kernel,maxm,exitcode" <+> ">times.csv"
        , splitcommand
          [ "$time/bin/time", "--format", dquotes ("$rulename,%e,%U,%S,%M,%x")
          , "--append", "--output", "times.csv"
          , "sh", "run.sh", "1>", ">(tee stdout)", "2>", ">(tee stderr >&2)"
          , "||:"]
        , "sed -i -e '/Command/d' 'times.csv'"
        ]
      )
    , ( "installPhase", mkStr "mkdir -p $out; mv * $out")
    ]

databaseExpr :: Set.Set InputFile -> NExpr
databaseExpr missing =
  Nix.mkFunction (
    Nix.mkParamset (
      [ ("stdenv", Nothing)
      , ("callPackage", Nothing)
      , ("nixec-builder", Nothing)
      ] ++ [ (packageToText (superPackage p), Nothing)
           | p <- toListOf (folded.inputFileInput._PackageInput) missing
           ]
      )
      False)
  $ Nix.mkSym "stdenv.mkDerivation" Nix.@@
  Nix.attrsE
  [ ("name", Nix.mkStr "database")
  , ("phases", Nix.mkStr "buildPhase")
  , ("buildInputs", Nix.mkList [Nix.mkSym "nixec-builder"])
  , ("db", Fix . Nix.NStr . Nix.DoubleQuoted $ concat
      [ [ Nix.Plain "type,value,file,output"]
      , concat
        [ ( Nix.Plain $
            "\n"
            <> ( Text.strip . Text.decodeUtf8 . BL.toStrict
                 $ Csv.encodeDefaultOrderedByNameWith
                 (Csv.defaultEncodeOptions { Csv.encIncludeHeader = False })
                 [i]
               )
            <> ","
          ) : inputFileToNixString i
        | i <- Set.toList missing
        ]
      ]
    )
  , ("buildPhase", mkIStr
      [ Nix.Plain "mkdir $out\n"
      , Nix.Plain "echo \"$db\" > $out/extra-paths.csv\n"
      , Nix.Plain "ln -s " , Nix.Antiquoted (Nix.mkRelPath "./."), Nix.Plain " $out/previous\n"
      , Nix.Plain "nixec-builder -v --db $out/previous/database.csv --db $out/extra-paths.csv $out"
      ]
    )
  ]
  where

    mkIStr = Fix . Nix.NStr . Nix.Indented 2

inputFileToNixString :: InputFile -> [Nix.Antiquoted Text.Text Nix.NExpr]
inputFileToNixString (InputFile i fp) =
  [ Nix.Antiquoted (toExpr i) ]
  ++ [ Nix.Plain (Text.pack $ "/" <> fp) | not $ null fp ]

ruleNameToFilePath :: RuleName -> FilePath -> FilePath
ruleNameToFilePath r fp =
  fp </> ruleNameToString r <.> "rule" <.> "nix"

class AsExpr a where
  toExpr :: a -> NExpr

instance AsExpr NExpr where
  toExpr = id

instance AsExpr a => AsExpr [a] where
  toExpr a = mkList (map toExpr a)

instance AsExpr Input where
  toExpr = \case
    PackageInput t -> toExpr t
    RuleInput r    -> callFileExpr (ruleNameToFilePath r "rules") []
    FileInput t    -> Nix.mkPath False t

instance AsExpr Package where toExpr = Nix.mkSym . packageToText
