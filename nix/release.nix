{ sources ? import ./sources.nix
, haskell-nix ? import sources.haskell-nix {}
, nixpkgs ? haskell-nix.sources.nixpkgs
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
}:

{
  epoxyHarden = import ./build.nix { inherit (pkgs) haskell-nix; };
}
