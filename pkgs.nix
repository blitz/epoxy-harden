{
  extras = hackage:
    {
      packages = {
        "elf" = (((hackage.elf)."0.30").revisions).default;
        epoxy-harden = ./epoxy-harden.nix;
        };
      };
  resolver = "lts-14.20";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }