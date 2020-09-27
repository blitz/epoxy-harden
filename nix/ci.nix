let release = import ./release.nix { };
in {
  epoxy-harden = release.epoxyHarden.epoxy-harden.components.exes.epoxy-harden;
  dhall = release.epoxyHarden.dhall.components.exes.dhall;
}
