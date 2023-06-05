{ pkgs ? import <nixpkgs> { } }:

# The protobuf derivation in nixpkgs does not include the conformance test
# runners.
# This protobuf derivation includes the conformance test runners.
#
# References
#
# https://github.com/protocolbuffers/protobuf/tags
#
# https://github.com/protocolbuffers/protobuf/blob/master/CHANGES.txt
#
# https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/libraries/protobuf
#
# https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats

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
  protobuf_repo_v3_20_1 = rec {
    ref = "v3.20.1";
    src = builtins.fetchGit {
      url = "https://github.com/protocolbuffers/protobuf";
      rev = "21027a27c4c2ec1000859ccbcfff46d83b16e1ed";
      ref = "refs/tags/${ref}";
    };
  };
  protobuf_repo_v3_21_0 = rec {
    ref = "v3.21.0";
    src = builtins.fetchGit {
      url = "https://github.com/protocolbuffers/protobuf";
      rev = "7062d0a2d0075d5e7d5c294fd3984df67a976da3";
      ref = "refs/tags/${ref}";
    };
  };
  protobuf_repo_v21_10 = rec {
    ref = "v21.10";
    src = builtins.fetchGit {
      url = "https://github.com/protocolbuffers/protobuf";
      rev = "11bc195441adfbc87ff0349f1c4be7a239d1daad";
      ref = "refs/tags/${ref}";
    };
  };
  protobuf_repo_v23_2 = rec {
    ref = "v23.2";
    src = pkgs.fetchFromGitHub {
      owner = "protocolbuffers";
      repo = "protobuf";
      rev = "v23.2";
      hash = "sha256-kYiT7MIhKH28YwZqaIj0ri/fJ5sOaUA4z9jM64o2iqg=";
      fetchSubmodules = true;
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
  mkProtobuf = repo: pkgs.stdenv.mkDerivation {
    name = "protobuf-${repo.ref}";
    nativeBuildInputs = with pkgs; [ autogen automake autoconf libtool rsync ];
    src = repo.src;
    # https://github.com/protocolbuffers/protobuf/blob/main/src/README.md#c-installation---unix
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
    meta = {
      description = "purescript-protobuf build for Google’s Protobuf";
      longDescription = ''
        This Protobuf build includes the protoc compiler and the “well-known”
        .proto definitions, as well as all of the conformance test runners
        and conformance .proto definitions.
        '';
      homepage = "https://github.com/xc-jp/purescript-protobuf";
      license = pkgs.lib.licenses.bsd3;
      mainProgram = "protoc";
    };
  };

  cmakeProtobuf = repo: pkgs.stdenv.mkDerivation {
    name = "protobuf-${repo.ref}";
    nativeBuildInputs = with pkgs; [ cmake rsync ];
    src = repo.src;
    # https://github.com/protocolbuffers/protobuf/blob/main/src/README.md#c-installation---unix
    cmakeFlags = [ "-Dprotobuf_BUILD_CONFORMANCE=ON" ];
    postInstall =
      let
        conformance_rpath = pkgs.lib.makeLibraryPath [
          pkgs.stdenv.cc.cc.lib
        ];
      in
      ''
      # Recursive copy all the .proto files
      rsync -am --include='*.proto' --include='*/' --exclude='*' $src/src $out/
      mkdir -p $out/conformance
      cp $src/conformance/conformance.proto $out/conformance/

      # Conformance test runner
      cp ./conformance_test_runner $out/bin/conformance_test_runner
      # https://nixos.wiki/wiki/Packaging/Binaries#Creating_the_Derivation_for_upstream_Packaging
      patchelf --set-interpreter "${pkgs.stdenv.cc.bintools.dynamicLinker}" --set-rpath "$out/lib:${conformance_rpath}" $out/bin/conformance_test_runner

      # Distribute the cpp conformance program as a test case
      cp ./conformance_cpp $out/bin/conformance_cpp
      patchelf --set-interpreter "${pkgs.stdenv.cc.bintools.dynamicLinker}" --set-rpath "$out/lib:${conformance_rpath}" $out/bin/conformance_cpp
      '';
    meta = {
      description = "Google’s Protobuf built for purescript-protobuf";
      longDescription = ''
        This Protobuf build includes the protoc compiler and the “well-known”
        .proto definitions, as well as all of the conformance test runners
        and conformance .proto definitions.
        '';
      homepage = "https://github.com/xc-jp/purescript-protobuf";
      license = pkgs.lib.licenses.bsd3;
      mainProgram = "protoc";
    };
  };

in
{
  inherit protobuf_repo_v3_9_2;
  inherit protobuf_repo_v3_14_0;
  inherit protobuf_repo_v3_15_8;
  inherit protobuf_repo_v3_20_1;
  inherit protobuf_repo_v3_21_0;
  inherit protobuf_repo_v21_10;
  inherit protobuf_repo_v23_2;
  protobuf_v3_9_2 = mkProtobuf protobuf_repo_v3_9_2;
  protobuf_v3_14_0 = mkProtobuf protobuf_repo_v3_14_0;
  protobuf_v3_15_8 = mkProtobuf protobuf_repo_v3_15_8;
  protobuf_v3_20_1 = mkProtobuf protobuf_repo_v3_20_1;
  protobuf_v3_21_0 = mkProtobuf protobuf_repo_v3_21_0;
  protobuf_v21_10 = mkProtobuf protobuf_repo_v21_10;
  protobuf_v23_2 = cmakeProtobuf protobuf_repo_v23_2;
}


# # This derivation is pretty much what's in nixpkgs as "protobuf", just `protoc`.
# protoc = pkgs.stdenv.mkDerivation {
#   name = "protoc-${version}";
#   buildInputs = [ pkgs.autoreconfHook ];
#   src = protobufRepo;
# };
