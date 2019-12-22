{
  extras = hackage:
    {
      packages = {
        "elf" = (((hackage.elf)."0.29").revisions).default;
        epoxy-harden = ./epoxy-harden.nix;
        };
      };
  resolver = "lts-14.17";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }