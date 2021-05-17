{ pkgs ? import <nixpkgs> { } }:

# References
#
# https://github.com/protocolbuffers/protobuf/tags
#
# https://github.com/protocolbuffers/protobuf/blob/master/CHANGES.txt
#
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/libraries/protobuf/generic-v3.nix
#
# https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats


# Path to the repo source code in the nix store
#
#     nix eval --raw -f nix/protobuf.nix protobuf_repo_v3_14_0.src.outPath
#

let

  protobuf_repo_v3_9_2 = rec {
    ref = "v3.9.2";
    src = builtins.fetchGit {
      url = "https://github.com/protocolbuffers/protobuf";
      rev = "52b2447247f535663ac1c292e088b4b27d2910ef";
      ref = "refs/tags/${ref}";
    };
  };
  protobuf_repo_v3_14_0 = rec {
    ref = "v3.14.0";
    src = builtins.fetchGit {
      url = "https://github.com/protocolbuffers/protobuf";
      rev = "2514f0bd7da7e2af1bed4c5d1b84f031c4d12c10";
      ref = "refs/tags/${ref}";
    };
  };
  protobuf_repo_v3_15_8 = rec {
    ref = "v3.15.8";
    src = builtins.fetchGit {
      url = "https://github.com/protocolbuffers/protobuf";
      rev = "436bd7880e458532901c58f4d9d1ea23fa7edd52";
      ref = "refs/tags/${ref}";
    };
  };

  # Builds `protoc`, plus the conformance test runners, and also copies
  # in the .proto files for the conformance test protocol,
  # especially `./src/google/protobuf/test_messages_proto3.proto`
  #
  # https://github.com/protocolbuffers/protobuf/tree/master/conformance
  #
  # See the Travis test runner script
  # https://github.com/protocolbuffers/protobuf/blob/master/tests.sh
  #
  # https://laptrinhx.com/an-elixir-library-for-protocol-buffers-2920413944/#conformance
  mkProtobuf = repo: pkgs.stdenv.mkDerivation {
    name = "protobuf-${repo.ref}";
    nativeBuildInputs = with pkgs; [ autogen automake autoconf libtool rsync ];
    src = repo.src;
    configurePhase = ''
      ./autogen.sh
      ./configure --prefix=$out
    '';
    buildPhase = ''
      make --jobs=$NIX_BUILD_CORES
      cd conformance
      make
      cd ..
    '';
    installPhase = ''
      mkdir -p $out
      make install
      cd conformance
      make install
      cd ..
      cp ./test-driver $out/bin/test-driver
      rsync -am --include='*.proto' --include='*/' --exclude='*' src $out/
      mkdir -p $out/conformance
      cp ./conformance/conformance.proto $out/conformance/
    '';
    LC_ALL = "C.UTF-8";
  };

in
{
  inherit protobuf_repo_v3_9_2;
  inherit protobuf_repo_v3_14_0;
  inherit protobuf_repo_v3_15_8;
  protobuf_v3_9_2 = mkProtobuf protobuf_repo_v3_9_2;
  protobuf_v3_14_0 = mkProtobuf protobuf_repo_v3_14_0;
  protobuf_v3_15_8 = mkProtobuf protobuf_repo_v3_15_8;
}


# # This derivation is pretty much what's in nixpkgs as "protobuf", just `protoc`.
# protoc = pkgs.stdenv.mkDerivation {
#   name = "protoc-${version}";
#   buildInputs = [ pkgs.autoreconfHook ];
#   src = protobufRepo;
# };
