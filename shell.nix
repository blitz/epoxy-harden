{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import nixpkgs (import sources.haskell-nix)
, lib ? pkgs.lib }:
pkgs.mkShell {
  buildInputs = [
    pkgs.niv
    pkgs.stack
    pkgs.haskell-nix.nix-tools
    # Broken :(
    # pkgs.stylish-haskell
    pkgs.hlint
  ];
}
