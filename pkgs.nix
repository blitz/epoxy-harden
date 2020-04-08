{
  extras = hackage:
    { packages = { epoxy-harden = ./epoxy-harden.nix; }; };
  resolver = "lts-15.7";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }