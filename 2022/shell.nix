let
  pkgs = import <nixpkgs> { };
in pkgs.mkShell {
  packages = [
             ];
  inputsFrom = [ (import ./release.nix).advent.env ];
}
