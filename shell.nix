{ pkgs ? import ./nix/pkgs.nix {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    purs
    easy-ps.spago
    nodejs
    easy-ps.pulp
    protobuf
    nodePackages.bower
    easy-ps.psc-package
    dhall
    dhall-json
    spago2nix
  ];
  shellHook = ''
  shopt -s globstar # Need for globbing packagePath in vscode PureScript IDE
  export PATH="./bin:./node_modules/.bin:$PATH"   # PATH to protoc-gen-purescript
  source <(spago --bash-completion-script `which spago`)
  echo "PureScript Protobuf development environment."
  protoc --version
  echo -n "purs "
  purs --version
  echo ""
  echo "To build the protoc compiler plugin, run:"
  echo ""
  echo "    spago -x spago-plugin.dhall build"
  echo ""
  echo "To generate PureScript .purs files from .proto files, run:"
  echo ""
  echo "    protoc --purescript_out=path_to_output *.proto"
  echo ""
  '';
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
