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

  protobuf = pkgs.stdenv.mkDerivation {
    name = "protobuf-${version}";
    buildInputs = [ pkgs.autoreconfHook ];
    src = protobufRepo;
  };

  protobufBuilt = pkgs.stdenv.mkDerivation {
    name = "protbufBuilt-${version}";
    # buildInputs = [pkgs.autoreconfHook ];
    # src = "${protobufRepo}/conformance";
    nativeBuildInputs = with pkgs; [ autogen automake autoconf libtool rsync ];
    src = protobufRepo;
    # unpackPhase = ''
    # '';
    # https://laptrinhx.com/an-elixir-library-for-protocol-buffers-2920413944/#conformance
    # preconfigure = ''
    #   ./autogen.sh
    #   '';
    configurePhase = ''
      ./autogen.sh
      ./configure --prefix=$out
      '';
    buildPhase = ''
      make -j$(_ncpus)
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
      # Copy the conformance test messages, especially ./src/google/protobuf/test_messages_proto3.proto
      rsync -am --include='*.proto' --include='*/' --exclude='*' src $out/
      '';
    };

  # https://github.com/protocolbuffers/protobuf/tree/master/conformance
  # See the Travis test runner script
  # https://github.com/protocolbuffers/protobuf/blob/master/tests.sh
  conformance = pkgs.stdenv.mkDerivation {
    name = "conformance-${version}";
    # buildInputs = [pkgs.autoreconfHook ];
    # src = "${protobufRepo}/conformance";
    nativeBuildInputs = [ pkgs.autogen pkgs.automake pkgs.autoconf pkgs.libtool];
    # src = protobufRepo;
    src = protobufBuilt;
    # # unpackPhase = ''
    # # '';
    # # https://laptrinhx.com/an-elixir-library-for-protocol-buffers-2920413944/#conformance
    # # preconfigure = ''
    # #   ./autogen.sh
    # #   '';
    # configurePhase = ''
    #   ./autogen.sh
    #   ./configure
    #   '';
    # # buildPhase = ''
    # #   # make -j
    # #   make
    # #   cd conformance
    # #   # make -j
    # #   make
    # #   cd ..
    # #   # ./tests.sh cpp
    # #   '';
    # # installPhase = ''
    # #   # mkdir -p $out/bin
    # #   # cp conformance/conformance-cpp $out/bin/conformance-cpp
    # #   # cp conformance/conformance-test-runner $out/bin/conformance-test-runner
    # #   # cp ./test-driver $out/bin/test-driver
    # #   #./install-sh -t $out
    # #   make dist
    # #   cd conformance
    # #   make dist
    # #   cd ..
    # #   '';
    # buildPhase = ''
    #   make
    #   cd conformance
    #   make
    #   cd ..
    #   '';
    installPhase = ''
      mkdir -p $out/bin
      cp ./conformance/conformance-cpp $out/bin/conformance-cpp
      cp ./conformance/conformance-test-runner $out/bin/conformance-test-runner
      cp ./test-driver $out/bin/test-driver
      mkdir -p $out/include
      cp ./conformance/conformance.proto $out/include/
      mkdir -p $out/include/google/protobuf
      cp ./src/google/protobuf/*.proto $out/include/google/protobuf/
      mkdir -p $out/lib/conformance
      cp ./conformance/conformance_test*.o $out/lib/conformance/
      rsync -am --include='*.proto' --include='*/' --exclude='*' . $out/
      '';

    # LANGUAGE = "en_US:en";
    # LC_ALL = (unset);
    # LANG = "en_US.utf8";

    LC_ALL = "C.UTF-8";
  };

in { inherit protobuf protobufBuilt conformance; }
