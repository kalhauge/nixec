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

  -- * Nix tools
  , nixBuild
  , nixBuildWithPkgsAndOverlays
  , nixBuildRules

  , nixInstantiate
  , nixInstantiateWithPkgsAndOverlays

  , nixShell
  , nixShellWithPkgsAndOverlays

  -- * Write rule
  , writeRule
  , writeDatabase

  , printExprWithPkgsAndOverlays
 
  -- * Expressions
  , databaseExpr
  , ruleExpr

  , oneRuleExpr
  , rulesExpr

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
import Data.Foldable
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
  , _nixSystem       :: Maybe (Text.Text)
  } deriving (Show, Eq)

makeClassy ''NixConfig


type HasNix env m = (MonadIO m, HasNixConfig env, MonadReader env m)

withArgs :: HasNix env m => [String] -> m a -> m a
withArgs _args =
  local (nixBuildCommand._2 %~ (++ _args))

-- | Call Nix-Build with an NExpr
nixBuild ::
  (HasNix env m, L.HasLogger env, AsExpr a)
  => a
  -> m [FilePath]
nixBuild (toExpr -> expr) = L.phase "build" $ do
  (_cmd, _args) <- view nixBuildCommand

  let script = show (prettyNix expr)

  priority <- view L.loggerPriority
  let
    a = proc _cmd $ concat
      [ _args
      , [ "-Q" | priority > L.DEBUG ]
      , [ "-E", script ]
      ]

  L.debug $ L.displayString script

  _stdout <- L.lineConsumer
  _stderr <- L.lineLogger L.INFO

  (exit, out, err) <- L.consume _stdout _stderr a

  case exit of
    ExitSuccess ->
      return . map (LazyText.unpack . LazyText.strip) $ appEndo out []
    e -> do
      L.warning $ "Build failed with " <> L.displayShow e
      let _list = appEndo err []
      forM_ (drop (List.length _list - 10) _list) $ L.info . L.displayLazyText
      return $ []

nixShell ::
  (HasNix env m, L.HasLogger env, AsExpr a)
  => a
  -> m ()
nixShell (toExpr -> expr) = do
  (_, _args) <- view nixBuildCommand

  let script = show (prettyNix expr)

  let
    shellProgram
      = setDelegateCtlc True
      $ proc "nix-shell" (_args ++ [ "-E", script ])

  L.info $ "Starting shell: "
  runProcess_ shellProgram

nixInstantiate ::
  (HasNix env m, L.HasLogger env, AsExpr a)
  => a
  -> m [FilePath]
nixInstantiate expr =
  local (nixBuildCommand._1 .~ "nix-instantiate") $ nixBuild expr

nixInstantiateWithPkgsAndOverlays ::
  (L.HasLogger env, HasNix env m, AsExpr a)
  => a
  -> m [FilePath]
nixInstantiateWithPkgsAndOverlays =
  nixInstantiate <=< withPkgsAndOverlaysExpr

nixShellWithPkgsAndOverlays ::
  (L.HasLogger env, HasNix env m, AsExpr a)
  => a
  -> m ()
nixShellWithPkgsAndOverlays =
  nixShell <=< withPkgsAndOverlaysExpr

-- | Convient wrapper around nixBuild and withPkgsAndOverlaysExpr
nixBuildWithPkgsAndOverlays ::
  (L.HasLogger env, HasNix env m, AsExpr a)
  => a
  -> m [FilePath]
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

  system <- view nixSystem
  let nixpkgs = mkSym "import"
        @@ mkEnvPath "nixpkgs"
        @@ attrsE [ ("system", mkStr m) | m <- toList system ]
  return $ mkWith (nixpkgs @. "appendOverlays" @@ mkList overlays) expr

nixBuildRules ::
  (HasNix env m, L.HasLogger env)
  => FilePath
  -> [RuleName]
  -> m [FilePath]
nixBuildRules folder rules =
  nixBuildWithPkgsAndOverlays (rulesExpr folder rules)

-- | 'callPackage <file> { .. }'
callFileExpr :: FilePath -> [ (Text.Text, NExpr) ] -> NExpr
callFileExpr = callPackageExpr . mkPath False

-- | 'callPackage <expr> { .. }'
callPackageExpr :: NExpr -> [ (Text.Text, NExpr) ] -> NExpr
callPackageExpr e b = mkSym "callPackage" @@ e @@ attrsE b

printExprWithPkgsAndOverlays :: (HasNix env m, AsExpr a) => a -> m ()
printExprWithPkgsAndOverlays =
  withPkgsAndOverlaysExpr >=> liftIO . print . prettyNix

-- | Write a rule to a folder
writeRule :: MonadIO m => FilePath -> Package -> RuleName -> Rule -> m ()
writeRule folder mkRule rn r = liftIO $ do
  let fn = ruleNameToFilePath (Left folder) rn
  createDirectoryIfMissing True (takeDirectory fn)
  writeFile fn . show . prettyNix $ ruleExpr mkRule rn r

-- | Write a rule to a folder
writeDatabase :: MonadIO m => FilePath -> NExpr -> Set.Set InputFile -> m ()
writeDatabase fn prev missing = liftIO $ do
  writeFile fn . show . prettyNix $ databaseExpr prev missing

rulesExpr :: FilePath -> [RuleName] -> [NExpr]
rulesExpr folder rules =
  [ oneRuleExpr folder rn | rn <- rules ]

oneRuleExpr :: FilePath -> RuleName -> NExpr
oneRuleExpr folder rn =
  callFileExpr (ruleNameToFilePath (Left folder) rn) []

ruleExpr :: Package -> RuleName -> Rule -> NExpr
ruleExpr mkRule rn r = mkFunction header (toExpr mkRule @@ body) where
  header = flip mkParamset False . map (,Nothing) . concat $
    [ [ "stdenv"
      , "callPackage"
      , "time"
      , "writeTextFile"
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
    , ( "phases", mkStr "configurePhase buildPhase installPhase")
    , ( "configurePhase", Fix . Nix.NStr . Nix.Indented 2 . List.intercalate [Plain "\n"] $
        [ [ Plain "# This is a comment to make sure that the output is put on multiple lines" ]
        , List.intercalate [Plain "\n"]
          [ case x of
              LinkTo fp i ->
                [ Plain "ln -s "]
                ++ inputFileToNixString (Right (ruleNameScope rn) ) i
                ++ [ Plain (Text.pack $ ' ': fp) ]
              CreateFile fp exe i ->
                [ Plain "ln -s "
                , Antiquoted
                  (mkSym "writeTextFile"
                    @@ attrsE
                    [ ("name", mkStr (Text.pack fp))
                    , ("text", mkStr i)
                    , ("executable", mkBool exe)
                    ]
                  )
                , Plain (Text.pack $ ' ': fp)
                ]
              OnPath _  -> []
              Env _ _  -> []
          | x <- r ^. ruleRequires
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
    , ( "shellHook", mkStr "cd $(mktemp -d); sh -c \"$configurePhase\";")
    ]

databaseExpr :: NExpr -> Set.Set InputFile -> NExpr
databaseExpr prev missing =
  mkFunction ( mkParamset ( map (,Nothing) $
      [ "stdenv" , "callPackage" , "nixec-builder"]
      ++ [ packageToText (superPackage p)
         | p <- toListOf (folded.inputFileInput._PackageInput) missing
         ]
      ) False)
  $ mkSym "stdenv.mkDerivation" @@ attrsE
  [ ("name", mkStr "database")
  , ("phases", mkStr "buildPhase")
  , ("buildInputs", mkList [Nix.mkSym "nixec-builder"])
  , ("db", Fix . NStr . DoubleQuoted $ concat
      [ [ Plain "type,value,file,output"]
      , concat
        [ ( Plain $
            "\n"
            <> ( Text.strip . Text.decodeUtf8 . BL.toStrict
                 $ Csv.encodeDefaultOrderedByNameWith
                 (Csv.defaultEncodeOptions { Csv.encIncludeHeader = False })
                 [i]
               )
            <> ","
          ) : inputFileToNixString (Left "rules") i
        | i <- Set.toList missing
        ]
      ]
    )
  , ("buildPhase", mkIStr
      [ Plain "mkdir $out\n"
      , Plain "echo \"$db\" > $out/extra-paths.csv\n"
      , Plain "ln -s " , Antiquoted (prev), Plain " $out/previous\n"
      , Plain "nixec-builder -v --previous $out/previous/database.nix --db $out/previous/database.csv --db $out/extra-paths.csv $out"
      ]
    )
  ]
  where

    mkIStr = Fix . Nix.NStr . Nix.Indented 2

inputFileToNixString :: Either FilePath Scope -> InputFile -> [Antiquoted Text.Text NExpr]
inputFileToNixString scp (InputFile i fp) =
  [ Antiquoted (inputToExprInScope scp i) ]
  ++ [ Plain (Text.pack $ "/" <> fp) | not $ null fp ]


-- | Given a scope determine the shortest path to the said
-- scope
diffScope :: Scope -> RuleName -> FilePath
diffScope f (RuleName to') =
  let (n, bs) = prefix (reverse f) (reverse (toList to'))
  in List.intercalate "/" (".":(map Text.unpack $ replicate n ".." ++ bs))
  where
    prefix al@(a:as) bl@(b:bs)
      | a == b = prefix as bs
      | otherwise =
        (length al, bl)
    prefix (_:as) [] =
      (1 + length as, [])
    prefix [] bs =
      (0, bs)


ruleNameToFilePath :: Either FilePath Scope -> RuleName -> FilePath
ruleNameToFilePath scp rn =
  case scp of
    Left fp -> fp </> ruleNameToString rn <.> "rule" <.> "nix"
    Right scp' -> diffScope scp' rn <.> "rule" <.> "nix"

class AsExpr a where
  toExpr :: a -> NExpr

instance AsExpr NExpr where
  toExpr = id

instance AsExpr a => AsExpr [a] where
  toExpr a = mkList (map toExpr a)

inputToExprInScope :: Either FilePath Scope -> Input -> NExpr
inputToExprInScope scp = \case
  PackageInput t -> toExpr t
  RuleInput rn   -> callFileExpr (ruleNameToFilePath scp rn) []
  FileInput t    -> Nix.mkPath False t

instance AsExpr Package where toExpr = Nix.mkSym . packageToText
