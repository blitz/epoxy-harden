{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources.haskell-nix {}
, nixpkgs ? haskell-nix.sources.nixpkgs
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
, lib ? pkgs.lib }:

let
  haskell = pkgs.haskell-nix;
  
  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    modules = [];
  };

  hsPkgs = pkgSet.config.hsPkgs;
in
{
  epoxyHarden = hsPkgs.epoxy-harden.components.exes.epoxy-harden;

  # Export the matching version of dhall as well to avoid incompatiblities.
  dhall = hsPkgs.dhall.components.exes.dhall;
}
