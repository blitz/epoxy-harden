{
  extras = hackage:
    {
      packages = {
        "elf" = (((hackage.elf)."0.30").revisions).default;
        "dhall" = (((hackage.dhall)."1.29.0").revisions).default;
        "prettyprinter" = (((hackage.prettyprinter)."1.5.1").revisions).default;
        epoxy-harden = ./epoxy-harden.nix;
        };
      };
  resolver = "lts-14.27";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }