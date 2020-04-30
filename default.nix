{
# The Nix package versions. It is important to fix the version so the 
# build is reproducable.
  pkgs ?  import nix/fix/nixpkgs.nix {}

# The Haskell compiler to use
, compiler ? "default"

# The nixec version to use. Override to use other version.
, nixec ? pkgs.lib.cleanSourceWith 
  { filter = (path: type: baseNameOf path != ".nix");
    src = pkgs.lib.cleanSource ./.;
  }
# The root of the evaluation, containing the ./Nixecfile.hs
# Here the nixecRoot is for developing
, root ? nixec

# The name of the evaluation
, name ? "nixec"

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
  pkgs = oldpkgs.extend (lib.composeExtensions nixecExtensions (self: deps));

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
    src    = root;
    phases = "buildPhase";
    buildInputs = [ 
      ( hpkgs2.ghcWithPackages build-packages )
    ];
    buildPhase = ''
      mkdir -p $out/bin
      ghc --make $src -rtsopts -threaded -with-rtsopts=-I0 \
           -XOverloadedStrings \
          -outputdir=$out/bin/ -o $out/bin/nixec-builder
    '';
  };

  database = pkgs.stdenv.mkDerivation {
    name        = "${name}-database";
    src         = root;
    buildInputs = [ builder pkgs.ghcid ];
    phases      = "buildPhase";
    buildPhase  = "nixec-builder database -o $out";
  };

  all = [ builder ];

  nixec = hpkgs2.nixec;
  
  deps = deps' pkgs;
}


