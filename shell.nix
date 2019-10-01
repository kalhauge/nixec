{ nixpkgs ? import <nixpkgs> {}}: nixpkgs.haskellPackages.callPackage ./default.nix {}
