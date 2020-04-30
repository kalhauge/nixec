{ pkgs ? import fix/nixpkgs.nix {}
, compiler ? "default"
, haskellPackages ? 
    if compiler == "default" 
    then pkgs.haskellPackages 
    else pkgs.haskell.packages."${compiler}"
, dirtree ? import fix/dirtree.nix
}: 
haskellPackages.developPackage {
  root = pkgs.lib.cleanSourceWith 
    { filter = path: type: baseNameOf path != ".nix";
      src = pkgs.lib.cleanSource ./.;
    };
  name = "nixec";
  source-overrides = {
    inherit dirtree;
  };
  modifier = drv:
    with pkgs.haskell.lib;
    addBuildTools drv (with haskellPackages; [ cabal-install ghcid ]);
}
