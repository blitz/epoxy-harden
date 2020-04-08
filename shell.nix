{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources.haskell-nix {}
, nixpkgs ? haskell-nix.sources.nixpkgs-default
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
, lib ? pkgs.lib }:
pkgs.mkShell {
  buildInputs = [
    # This can be replaced with pkgs.niv once nixpkgs is new enough.
    (import sources.niv {}).niv

    pkgs.stack
    pkgs.haskell-nix.nix-tools
    # Broken :(
     pkgs.stylish-haskell
    pkgs.hlint
  ];
}
