{ haskell-nix }:

haskell-nix.stackProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "epoxy-harden";
    src = ./..;
  };
}
