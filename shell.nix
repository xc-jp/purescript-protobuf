{ pkgs ? import ./nix/pkgs.nix {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    easy-ps.purs-0_14_3
    easy-ps.spago
    nodejs-14_x
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
