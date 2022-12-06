let
  pkgs = import <nixpkgs> { };
in pkgs.mkShell {
  packages = [ pkgs.haskellPackages.fourmolu
               pkgs.haskell-language-server
             ];
  inputsFrom = [ (import ./release.nix).advent.env ];
}
