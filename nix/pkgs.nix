# https://nixos.org/guides/towards-reproducibility-pinning-nixpkgs.html

{ pkgs ? import (builtins.fetchGit {
  # https://github.com/NixOS/nixpkgs/releases/tag/20.09
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/tags/20.09";
  rev = "cd63096d6d887d689543a0b97743d28995bc9bc3";
  }) {}
}:
let
  easy-ps = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/easy-purescript-nix.git";
    rev = "bbef4245cd6810ea84e97a47c801947bfec9fadc";
  }) { inherit pkgs; };


  spago2nix = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/spago2nix.git";
    # rev = "898798204fa8f53837bbcf71e30aeb425deb0906";
    ref = "spago-pass-args";
    rev = "3bf9161ba1bf329e682108405daf8f3073990515";
  }) { inherit pkgs; };

  node2nix = import (builtins.fetchGit {
    url = "https://github.com/svanderburg/node2nix.git";
    rev = "0c94281ea98f1b17532176106f90f909aa133704";
  }) { inherit pkgs; };

  protobuf = (import ./protobuf.nix { inherit pkgs; }).protobuf_v3_15_8;

in pkgs //
  { inherit easy-ps;
    inherit spago2nix;
    inherit node2nix;
    inherit protobuf;
    purs = easy-ps.purs-0_14_3;
    nodejs = pkgs.nodejs-14_x;
  }
