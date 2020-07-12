{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources.haskell-nix {}
, nixpkgs ? haskell-nix.sources.nixpkgs
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
, lib ? pkgs.lib }:
pkgs.mkShell {
  buildInputs = [
    # Build Tools (required for development)
    pkgs.stack
    pkgs.haskell-nix.nix-tools

    # Linters and Formatters (optional)
    pkgs.stylish-haskell
    pkgs.stylish-cabal
    pkgs.hlint
  ];
}
