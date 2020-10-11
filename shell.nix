{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources.haskell-nix {}
, nixpkgs ? haskell-nix.sources.nixpkgs
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
}:
let
  release =
    import ./nix/release.nix { inherit sources haskell-nix nixpkgs pkgs; };
in
release.epoxyHarden.shellFor {
  packages = ps: [ ps.epoxy-harden ];

  tools = { cabal = "3.2.0.0"; hlint = "3.1.6"; };

  buildInputs = [
    # Build Tools (required for development)
    pkgs.niv
    pkgs.stack
  ];
}
