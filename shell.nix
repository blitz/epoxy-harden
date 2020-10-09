{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources.haskell-nix {}
, nixpkgs ? haskell-nix.sources.nixpkgs
, pkgs ? import nixpkgs haskell-nix.nixpkgsArgs
, unstablePkgs ? import sources.nixpkgs-unstable {}
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
    unstablePkgs.stack
    pkgs.haskell-nix.nix-tools.ghc884

    # Editor Integration (optional)
    #pkgs.haskellPackages.ghcide
    #pkgs.haskellPackages.hspec-discover
    unstablePkgs.haskellPackages.haskell-language-server

    # Linters and Formatters (optional)
    pkgs.stylish-haskell
    pkgs.stylish-cabal
  ];
}
