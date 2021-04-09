{ pkgs ? import <nixpkgs> { } }:

# https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/libraries/protobuf/generic-v3.nix

# https://github.com/protocolbuffers/protobuf/blob/master/CHANGES.txt
let
  # version = "v3.9.2";
  # sha256 = "080zxa9w1pxp5y05aiwc0c8mlqkkh98wmid4l7m99cliphsd4qnn";
  # version = "v3.11.4";
  # sha256 = "00g61f1yd8z5l0z0svmr3hms38ph35lcx2y7hivw6fahslw0l8yw";
  # version = "v3.13.0";
  # sha256 = "1nqsvi2yfr93kiwlinz8z7c68ilg1j75b2vcpzxzvripxx5h6xhd";
  version = "v3.14.0";
  sha256 = "1k4kkb78kdbz732wsph07v3zy3cz7l1msk2byrfvp0nb02sfl3a4";
  # version = "v3.15.6";
  # sha256 = "15mba1hxv2gmlljiwh4kvjw2s1s4cf470kvx9lvfhaklskjig1l5";

  protobufRepo = pkgs.fetchFromGitHub {
    owner = "protocolbuffers";
    repo = "protobuf";
    rev = version;
    inherit sha256;
  };

  # # This is pretty much what's in nixpkgs as "protobuf", just `protoc`.
  # protoc = pkgs.stdenv.mkDerivation {
  #   name = "protoc-${version}";
  #   buildInputs = [ pkgs.autoreconfHook ];
  #   src = protobufRepo;
  # };

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
  protobuf = pkgs.stdenv.mkDerivation {
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

in protobuf
