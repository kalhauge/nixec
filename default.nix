{
# The Nix package versions. It is important to fix the version so the 
# build is reproducable.
  pkgs ?  import nix/fix/nixpkgs.nix {}

# The Haskell compiler to use
, compiler ? "default"

# The nixec version to use. Override to use other version.
, nixec ? pkgs.lib.cleanSourceWith 
  { filter = (path: type: ! (pkgs.lib.hasSuffix ".nix" path));
    src = pkgs.lib.cleanSource ./.;
  }
# The root of the evaluation, containing the ./Nixecfile.hs
# Here the nixecRoot is for developing
, nixecfile 

# The name of the evaluation
, name ? "nixec"

# The current database
, db ? null

# List of haskell source folders that is used in the ./Nixecfile.hs
, source-overrides ? {}

# Haskell packages needed to build the ./Nixecfile.hs:
, build-packages ? (hpkgs: with hpkgs; [ hpkgs.nixec ])

# Postprocessing
, postprocess ? {}

# Evaluation dependencies. This is where we should put our depenencies, for the
# entir evaluation.
, deps ? pkgs: {}
}:
let 
  oldpkgs = pkgs;

  inherit (oldpkgs) lib haskell;

  nixecExtensions = self: super: {
  };

in let 
  pkgs = oldpkgs.extend (lib.composeExtensions nixecExtensions (self: super: deps self));

  hpkgs = 
    ( if compiler == "default" 
      then pkgs.haskellPackages 
      else pkgs.haskell.packages."${compiler}"
    );

  hpkgs2 = hpkgs.extend (
    lib.composeExtensions 
      ( haskell.lib.packageSourceOverrides
      ({ nixec = nixec; 
         dirtree = import nix/fix/dirtree.nix;
       } // source-overrides)
      )
      (self: super: {}
      )
    );

    deps' = deps;
    nixec' = nixec;
in rec { 
  builder = pkgs.stdenv.mkDerivation { 
    name   = "${name}-builder";
    src    = nixecfile;
    phases = "buildPhase";
    buildInputs = [ 
      ( hpkgs2.ghcWithPackages build-packages ) nixec
    ];
    buildPhase = ''
      mkdir -p $out/bin
      ghc -o $out/bin/nixec-builder \
          --make $src -rtsopts -threaded -with-rtsopts=-I0 \
          -XOverloadedStrings \
          -outputdir=$out/bin/
    '';
  };

  database = 
    pkgs.stdenv.mkDerivation {
      name        = "${name}-database";
      buildInputs = [ builder pkgs.ghcid ];
      phases      = "buildPhase";
      buildPhase  = ''
        ${lib.optionalString (db != null) "echo 'Using-missing: ${db}'"}
        nixec-builder -v ${if db != null then "--db " + pkgs.callPackage db {} else "" } $out
      '';
    };

  all = [ builder ];

  nixec = hpkgs2.nixec;

  deps = deps' pkgs;
}


