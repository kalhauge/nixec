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
    [ ("name", mkStr (topRuleName rn))
    ]


-- ruleToNixDrv ::
--   Package -> RuleName -> Rule -> Doc e
-- ruleToNixDrv mkRule rn r =
--   pretty mkRule <+>
--     attrset
--     [ ( "name"
--       , nixstring (ruleNameToNix rn)
--       )
--     , ( "buildInputs"
--       , encloseSep "[" "]" " "
--         [ pretty p
--         | OnPath p <- r ^. ruleRequires
--         ]
--       )
--     , ( "command"
--       , "builtins.toFile" <+> dquotes "run.sh" <+>
--         script (renderCommands (r ^. ruleCommands))
--       )
--     , ( "unpackPhase"
--       , ( script . vcat . concat $
--           [ [ "ln -s" <+> inputToShell i <+> squotes (pretty fp)
--             | LinkTo fp i <- r ^. ruleRequires
--             ]
--           , [ "ln -s $command run.sh"
--             ]
--           ]
--         )
--       )
--     , ( "buildPhase"
--       , script . vcat . concat $
--         [ [ "export " <> pretty n <> "=" <> dquotes (inputToShell i)
--           | Env n i <- r ^. ruleRequires
--           ]
--         -- , [ "timeout" <+> pretty timelimit ]
--         , [ "echo" <+> dquotes "rule,real,user,kernel,maxm,exitcode" <+> ">times.csv"
--           , splitcommand
--             [ "${time}/bin/time", "--format"
--             , dquotes ((pretty rn) <> ",%e,%U,%S,%M,%x")
--             , "--append", "--output", "times.csv"
--             , "sh", "run.sh", "1>", ">(tee stdout)", "2>", ">(tee stderr >&2)"
--             , "||:"]
--           , "sed -i -e '/Command/d' 'times.csv'"
--           ]
--         ]
--       )
--     , ( "installPhase"
--       , script . vcat $
--         [ "mkdir -p $out"
--         , "mv * $out"
--         ]
--       )
--     ]
--   where
--     script :: Doc m -> Doc m
--     script a = vcat $ [ "''", indent 2 a , "''"]

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
      , Nix.Plain "nixec-builder --db $out/previous/database.csv --db $out/extra-paths.csv $out"
      ]
    )
  ]
  where

    mkIStr = Fix . Nix.NStr . Nix.Indented 2

inputFileToNixString :: InputFile -> [Nix.Antiquoted Text.Text Nix.NExpr]
inputFileToNixString (InputFile i fp) =
  [ Nix.Antiquoted (toExpr i) ]
  ++ [ Nix.Plain (Text.pack $ "/" <> fp) | not $ null fp ]

-- nixstring :: Doc m -> Doc m
-- nixstring = enclose
--   (flatAlt "\"" ("''" <> hardline))
--   (flatAlt "\"" (hardline <> "''"))

-- nixPackageScript' ::
--   HasNix env m
--   => Nix.NExpr
--   -> m String
-- nixPackageScript' pkg' = do
--   fps <- view nixOverlays

--   overlays <- fmap concat . forM fps $ \fp -> do
--    liftIO (doesFileExist fp) <&> \b ->
--      [ Nix.mkSym "import" Nix.@@ Nix.mkPath False fp | b ]

--   return . show . Nix.prettyNix $
--     Nix.mkWith
--       ((Nix.mkSym "import" Nix.@@ (Nix.mkEnvPath "nixpkgs") Nix.@@ Nix.attrsE []) Nix.@. "appendOverlays"
--         Nix.@@ Nix.mkList overlays)
--       pkg'

-- ruleNameToNix :: RuleName -> Doc m
-- ruleNameToNix =
--   foldr1 (\a b -> b <> "-" <> a) . fmap pretty . unRuleName

-- ruleNameToCallPackage :: RuleName -> Doc m
-- ruleNameToCallPackage rn =
--   "callPackage" <+> "./" <> ruleNameToNix rn <> ".nix" <+> "{}"

-- nixToShell :: Doc m -> Doc m
-- nixToShell = enclose "${" "}"

-- inputToShell :: InputFile -> Doc m
-- inputToShell = \case
--   InputFile a fp ->
--     case a of
--       RuleInput r -> nixToShell (ruleNameToCallPackage r)
--       PackageInput r -> nixToShell (pretty r)
--       FileInput r -> nixToShell ("../../" <> pretty r)
--     <> (if null fp then mempty else "/" <> pretty fp )

-- attrset :: [(Doc m, Doc m)] -> Doc m
-- attrset =
--   braces
--   . enclose line line
--   . indent 2
--   . vsep
--   . map (\(n, m) -> n <+> "=" <+> m <> ";")

-- letlist :: [(Doc m, Doc m)] -> Doc m -> Doc m
-- letlist lst inexp =
--   "let"
--   <+> ( enclose softline softline
--        . indent 2
--        . vsep
--        . map (\(n, m) -> n <+> "=" <+> m <> ";")
--        $ lst
--      )
--   <+> "in"
--   <+> inexp


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
