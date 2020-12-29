{ haskell-nix }:

haskell-nix.stackProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "epoxy-harden";
    src = ./..;
  };

  index-state = "2020-12-29T00:00:00Z";
  stack-sha256 = "1b65r1k5js5zgiwq1g2y59b506zlczm1p6vlhx5hpq5qlqqs4pzk";
}
