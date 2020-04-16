{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources.haskell-nix {}
, nixpkgs ? haskell-nix.sources.nixpkgs-default
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
, lib ? pkgs.lib }:
pkgs.mkShell {
  buildInputs = [
    # This can be replaced with pkgs.niv once nixpkgs is new enough.
    (import sources.niv {}).niv

    # Build Tools (required for development)
    pkgs.stack
    pkgs.haskell-nix.nix-tools

    # Linters and Formatters (optional)
    pkgs.stylish-haskell
    pkgs.stylish-cabal
    pkgs.hlint
  ];
}
