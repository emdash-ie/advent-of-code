#!/usr/bin/env fish
cabal2nix . > advent.nix
and nix-shell --run "cabal $argv"
