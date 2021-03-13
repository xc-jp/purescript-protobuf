# https://nixos.org/guides/towards-reproducibility-pinning-nixpkgs.html
{ pkgs ? import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
  sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {}
}:

let
  easy-ps = import
    (builtins.fetchGit {
      url = "https://github.com/justinwoo/easy-purescript-nix.git";
      rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
    }) { inherit pkgs; };

  spago2nix = import (builtins.fetchGit {
    url = "https://github.com/xc-jp/spago2nix.git";
    ref = "ls-transitive";
    rev = "1e65ec78def30b9c10bfc165cfb5f3b1c42bbd80";
  }) { inherit pkgs; };

  node2nix = import (builtins.fetchGit {
    url = "https://github.com/svanderburg/node2nix.git";
    rev = "c6cc7edc7c180ef01f23bc2aff31a1cbb5524b0d";
  }) { inherit pkgs; };

in pkgs // { inherit easy-ps; inherit spago2nix; inherit node2nix; }
