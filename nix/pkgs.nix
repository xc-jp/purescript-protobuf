# https://nixos.org/guides/towards-reproducibility-pinning-nixpkgs.html
# { pkgs ? import <nixpkgs> { } }:
{ pkgs ? import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
  sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {}
}:

let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
      sha256 = "12hk2zbjkrq2i5fs6xb3x254lnhm9fzkcxph0a7ngxyzfykvf4hi";
    }) { inherit pkgs; };

  spago2nix = import (builtins.fetchGit {
    #url = "git@github.com:justinwoo/spago2nix.git";
    #rev = "22bd974ec44c98d2831c31f4f4402a9afa8aecea";
    url = "git@github.com:xc-jp/spago2nix.git";
    ref = "ls-transitive";
    rev = "1e65ec78def30b9c10bfc165cfb5f3b1c42bbd80";
  }) { inherit pkgs; };

  node2nix = import (builtins.fetchGit {
    url = "git@github.com:svanderburg/node2nix.git";
    rev = "c6cc7edc7c180ef01f23bc2aff31a1cbb5524b0d";
  }) { inherit pkgs; };

in pkgs // { inherit easy-ps; inherit spago2nix; inherit node2nix; }
