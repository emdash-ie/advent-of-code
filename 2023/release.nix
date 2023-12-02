let
  pkgs = import <nixpkgs> { };

in
  { advent = pkgs.haskellPackages.callPackage ./advent.nix { };
  }
