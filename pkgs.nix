{
  extras = hackage:
    {
      packages = {
        "dhall" = (((hackage.dhall)."1.32.0").revisions).default;
        epoxy-harden = ./epoxy-harden.nix;
        };
      };
  resolver = "lts-15.13";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }