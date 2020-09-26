{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources.haskell-nix { }
, nixpkgs ? haskell-nix.sources.nixpkgs
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
}:

let
  release =
    import ./nix/release.nix { inherit sources haskell-nix nixpkgs pkgs; };
in release.epoxyHarden.epoxy-harden.components.exes.epoxy-harden
