{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import nixpkgs (import sources.haskell-nix)
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
