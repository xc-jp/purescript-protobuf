{ pkgs ? import ./nix/pkgs.nix {} }:
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.easy-ps.purs-0_13_8
    pkgs.easy-ps.spago
    pkgs.nodejs-13_x
    pkgs.easy-ps.pulp
    pkgs.protobuf3_9
    pkgs.nodePackages.bower
    pkgs.easy-ps.psc-package
    pkgs.dhall
    pkgs.dhall-json
  ];
  shellHook = ''
  export PATH="./bin:$PATH"   # PATH to protoc-gen-purescript
  echo "Purescript Protobuf development environment."
  echo "To build purescript-protobuf, run:"
  echo ""
  echo "    npm install"
  echo "    spago build"
  echo ""
  echo "To generate Purescript .purs files from .proto files, run:"
  echo ""
  echo "    protoc --purescript_out=path_to_output *.proto"
  echo ""
  '';
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
