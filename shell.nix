{ nixpkgs ? import <nixpkgs> {}}: nixpkgs.haskellPackages.mkGhcidShell ./default.nix
