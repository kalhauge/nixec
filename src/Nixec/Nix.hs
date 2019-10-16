{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Nixec.Nix where

-- lens
import Control.Lens hiding ((<.>))

-- prettyprinter
import Data.Text.Prettyprint.Doc

-- containers
import qualified Data.Set as Set

-- data-fix
import Data.Fix

-- filepath
import System.FilePath

-- hnix
import qualified Nix

-- nixec
import Nixec.Rule
import Nixec.Command
import Nixec.Data
import qualified Nixec.Logger as L

-- filepath
-- import System.FilePath

-- cassava
import qualified Data.Csv as Csv

-- directory
import System.Directory

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- import Data.Text.Encoding

-- process
import System.Process

-- base
import Control.Monad.IO.Class
import Control.Exception
import System.IO
-- import Data.String
-- import Data.Foldable
-- import System.Exit
-- import qualified Data.List as List

-- mtl
import Control.Monad.Reader

data NixConfig = NixConfig
  { _nixOverlays     :: [ FilePath ]
  , _nixVerbose      :: Bool
  , _nixBuildCommand :: (String, [ String ])
  } deriving (Show, Eq)

makeClassy ''NixConfig

type HasNix env m = (MonadIO m, HasNixConfig env, MonadReader env m)

-- writeNixRule :: HasNix env m
--   => RuleName
--   -> Rule
--   -> m ()
-- writeNixRule rn r = do
--   file <- nixFile rn
--   liftIO . createDirectoryIfMissing True =<< view nixFolder
--   mkRule <- view nixMkRule
--   liftIO . writeFile file . show $ ruleToNix mkRule rn r

-- nixFile :: (MonadReader env m, HasNixConfig env) => RuleName -> m FilePath
-- nixFile rn =
--   view nixFolder <&> \folder ->
--   folder </> show (ruleNameToNix rn) <.> "nix"

nixBuild ::
  (HasNix env m, L.HasLogger env)
  => String
  -> m (Maybe FilePath)
nixBuild script = do
  (_cmd, _args) <- view nixBuildCommand
  verbose <- view nixVerbose

  let a = proc _cmd (_args ++ [ "-E", script ])
  L.info $ "Building script: "
  L.info $ L.displayString script
  out <- liftIO . try $ do
    bracket (openFile "/dev/null" WriteMode) hClose $ \devnull -> do
      let a' = a { delegate_ctlc = False
                , std_err = if verbose then Inherit else UseHandle devnull
                }
      readCreateProcess a' ""

  case out of
    Right s ->
      return $ Just (Text.unpack . Text.strip . Text.pack $ s)
    Left (msg :: IOException) -> do
      L.warning $ "Build failed with " <> L.displayShow msg
      return $ Nothing

withArgs :: HasNix env m => [String] -> m a -> m a
withArgs _args =
  local (nixBuildCommand._2 %~ (++ _args))

nixPackageScript' ::
  HasNix env m
  => Nix.NExpr
  -> m String
nixPackageScript' pkg' = do
  fps <- view nixOverlays

  overlays <- fmap concat . forM fps $ \fp -> do
   liftIO (doesFileExist fp) <&> \b ->
     [ Nix.mkSym "import" Nix.@@ Nix.mkPath False fp | b ]

  return . show . Nix.prettyNix $
    Nix.mkWith
      ((Nix.mkSym "import" Nix.@@ (Nix.mkEnvPath "nixpkgs") Nix.@@ Nix.attrsE []) Nix.@. "appendOverlays"
        Nix.@@ Nix.mkList overlays)
      pkg'

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
  Package -> RuleName -> Rule -> Doc e
ruleToNix mkRule rn r =
  header <> ":" <> line <> ruleToNixDrv mkRule rn r
  where
  header = encloseSep "{ " " }" ", " $
    ["stdenv", "callPackage", "time"]
    ++
    [ pretty (superPackage p)
    | p <- mkRule:toListOf (folding ruleInputs.cosmosOf (_InInput._1)._PackageInput) r
    ]

ruleToNixDrv ::
  Package -> RuleName -> Rule -> Doc e
ruleToNixDrv mkRule rn r =
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

nixMissing :: Set.Set Input -> Doc m
nixMissing missing = Nix.prettyNix $
  Nix.mkFunction (
    Nix.mkParamset (
      [ ("stdenv", Nothing)
      , ("callPackage", Nothing)
      , ("nixec-builder", Nothing)
      ] ++ [ (packageToText (superPackage p), Nothing)
           | p <- toListOf (folded.cosmosOf (_InInput._1)._PackageInput) missing
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
          ) : inputToNix i
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
    inputToNix = \case
      PackageInput t ->
        [ Nix.Antiquoted $ Nix.mkSym (packageToText t) ]
      RuleInput t ->
        [ Nix.Antiquoted $
          nixCallFile ("rules" </> ruleNameToString t <.> "nix")
        ]
      FileInput t ->
        [ Nix.Antiquoted $ Nix.mkPath False t ]
      InInput t b ->
        inputToNix t ++ [ Nix.Plain (Text.pack $ "/" <> b) ]
  
    mkIStr = Fix . Nix.NStr . Nix.Indented 2


nixBuildRules :: (L.HasLogger env, HasNix env m)
  => FilePath
  -> [RuleName]
  -> m (Maybe FilePath)
nixBuildRules folder rules = do
  scrpt <- nixPackageScript'
        $ Nix.mkList [ nixCallFile (folder </> ruleNameToString t <.> "nix") | t <- rules ]
  nixBuild scrpt


nixCallFile :: FilePath -> Nix.NExpr
nixCallFile fp =
  Nix.mkSym "callPackage"
  Nix.@@ Nix.mkPath False fp Nix.@@ Nix.attrsE []

nixstring :: Doc m -> Doc m
nixstring = enclose
  (flatAlt "\"" ("''" <> hardline))
  (flatAlt "\"" (hardline <> "''"))
