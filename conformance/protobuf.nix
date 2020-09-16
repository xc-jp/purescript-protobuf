{ pkgs ? import <nixpkgs> { } }:

# https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats

let
  # version = "v3.13.0";
  # sha256 = "1nqsvi2yfr93kiwlinz8z7c68ilg1j75b2vcpzxzvripxx5h6xhd";
  version = "v3.9.2";
  sha256 = "080zxa9w1pxp5y05aiwc0c8mlqkkh98wmid4l7m99cliphsd4qnn";

  protobufRepo = pkgs.fetchFromGitHub {
    owner = "protocolbuffers";
    repo = "protobuf";
    rev = version;
    inherit sha256;
  };

  # This is pretty much what's in nixpkgs as "protobuf", just `protoc`.
  protoc = pkgs.stdenv.mkDerivation {
    name = "protoc-${version}";
    buildInputs = [ pkgs.autoreconfHook ];
    src = protobufRepo;
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
  conformance = pkgs.stdenv.mkDerivation {
    name = "conformance-${version}";
    nativeBuildInputs = with pkgs; [ autogen automake autoconf libtool rsync ];
    src = protobufRepo;
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

in { inherit protoc conformance; }
