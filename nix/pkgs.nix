# https://nixos.org/guides/towards-reproducibility-pinning-nixpkgs.html
# { pkgs ? import <nixpkgs> { } }:
{ nixpkgs ? import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
  sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {}
}:

let
  easy-ps = import
    (nixpkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
      sha256 = "12hk2zbjkrq2i5fs6xb3x254lnhm9fzkcxph0a7ngxyzfykvf4hi";
    }) {
    pkgs = nixpkgs;
  };

# in { inherit nixpkgs; inherit easy-ps; }
in nixpkgs // { inherit easy-ps; }
