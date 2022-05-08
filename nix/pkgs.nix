# https://nixos.org/guides/towards-reproducibility-pinning-nixpkgs.html

{ pkgs ? import (builtins.fetchGit {
  # https://github.com/NixOS/nixpkgs/releases/tag/20.09
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/tags/21.11";
  rev = "a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31";
  }) {}
}:
let
  easy-ps = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/easy-purescript-nix.git";
    ref = "master";
    rev = "0ad5775c1e80cdd952527db2da969982e39ff592";
  }) { inherit pkgs; };


  spago2nix = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/spago2nix.git";
    ref = "master";
    rev = "1c834738a8216a4c89d9defac9bf1c331d302a6a";
  }) { inherit pkgs; };

  node2nix = import (builtins.fetchGit {
    url = "https://github.com/svanderburg/node2nix.git";
    ref = "master";
    rev = "68f5735f9a56737e3fedceb182705985e3ab8799";
  }) { inherit pkgs; };

  protobuf = (import ./protobuf.nix { inherit pkgs; }).protobuf_v3_20_1;

in pkgs //
  { inherit easy-ps;
    inherit spago2nix;
    inherit node2nix;
    inherit protobuf;
    purs = easy-ps.purs-0_15_0;
    nodejs = pkgs.nodejs-17_x;
  }
