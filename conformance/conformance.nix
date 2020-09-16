{ pkgs ? import <nixpkgs> { } }:

# https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats

let
  version = "v3.13.0";

  protobufRepo = pkgs.fetchFromGitHub {
    owner = "protocolbuffers";
    repo = "protobuf";
    rev = version;
    sha256 = "1nqsvi2yfr93kiwlinz8z7c68ilg1j75b2vcpzxzvripxx5h6xhd";
  };

  protobuf = pkgs.stdenv.mkDerivation {
    name = "protobuf-${version}";
    buildInputs = [ pkgs.autoreconfHook ];
    src = protobufRepo;
  };

  conformance = pkgs.stdenv.mkDerivation {
    name = "conformance-${version}";
    # buildInputs = [pkgs.autoreconfHook ];
    # src = "${protobufRepo}/conformance";
    nativeBuildInputs = [ pkgs.autogen pkgs.automake pkgs.autoconf pkgs.libtool];
    src = protobufRepo;
    # unpackPhase = ''
    # '';
    # https://laptrinhx.com/an-elixir-library-for-protocol-buffers-2920413944/#conformance
    # preconfigure = ''
    #   ./autogen.sh
    #   '';
    configurePhase = ''
      ./autogen.sh
      ./configure
      '';
    buildPhase = ''
      make -j
      cd conformance
      # make -j
      make
      '';
    installPhase = ''
      mkdir -p $out/bin
      cp conformance/conformance-cpp $out/conformance-cpp
      cp conformance/conformance-test-runner $out/conformance-test-runner
      cp ./test-driver $out/test-driver
      '';

    # LANGUAGE = "en_US:en";
    # LC_ALL = (unset);
    # LANG = "en_US.utf8";

    LC_ALL = "C.UTF-8";
  };

in { inherit protobuf conformance; }
