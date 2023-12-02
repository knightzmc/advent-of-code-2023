{ pkgs ? import <nixpkgs> {} }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
in
pkgs.haskell.packages.ghc94.shellFor {
  packages = _: [ (pkgs.haskellPackages.callCabal2nix "aoc" src {}) ];
  buildInputs = with pkgs; [
    haskell.packages.ghc94.haskell-language-server
    cabal-install
    hlint
    haskell.packages.ghc94.fourmolu
  ];
}