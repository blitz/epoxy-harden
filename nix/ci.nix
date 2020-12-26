{ sources ? import ./sources.nix, haskell-nix ? import sources.haskell-nix { }
, nixpkgs ? haskell-nix.sources.nixpkgs
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs }:
let
  release = import ./release.nix { inherit sources haskell-nix nixpkgs pkgs; };
  epoxy-harden = release.epoxyHarden.epoxy-harden;
in {
  epoxy-harden = epoxy-harden.components.exes.epoxy-harden;
  epoxy-dtb = epoxy-harden.components.exes.epoxy-dtb;

  # I'm too stupid to use the built-in support for running unit tests
  # in haskell.nix, so let's do it the manual way.
  tests = let testDrv = epoxy-harden.components.tests.epoxy-harden-test;
  in pkgs.runCommand "epoxy-harden-test" { } ''
    ${testDrv}/bin/epoxy-harden-test | tee $out
  '';

  dhall = release.epoxyHarden.dhall.components.exes.dhall;
}
