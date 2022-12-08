# taken from NixOS/nixpkgs
# https://github.com/NixOS/nixpkgs/blob/379690ba90695a6cefe20c2d51dfcd44a1e8b562/pkgs/tools/package-management/nix-prefetch-scripts/default.nix
#
# We patch nix-prefetch-git so that it doesn't call nix-store --add-fixed,
# because it requires access to the Nix daemon, which is not accessible
# in a build phase for spago2nix.
{ lib, stdenv, stdenvNoCC, gnused, nix, coreutils, findutils, gawk, git, path, makeWrapper, ... }:
let
  mkPrefetchScript = tool: src: deps:
    stdenv.mkDerivation {
      name = "nix-prefetch-${tool}";

      nativeBuildInputs = [ makeWrapper ];

      dontUnpack = true;

      installPhase = ''
        install -vD ${src} $out/bin/$name;
        wrapProgram $out/bin/$name \
          --prefix PATH : ${lib.makeBinPath (deps ++ [ gnused nix ])} \
          --set HOME /homeless-shelter
      '';

      preferLocalBuild = true;
    };
  patched-file = stdenvNoCC.mkDerivation {
    src = "${path}/pkgs/build-support/fetchgit";
    name = "nix-prefetch-git-patched";
    patches = [ ./nix-prefetch-git-no-store.patch ];
    phases = [ "unpackPhase" "patchPhase" "installPhase" ];
    installPhase = ''
      cp nix-prefetch-git $out
    '';
  };
in
mkPrefetchScript "git" patched-file [ coreutils findutils gawk git ]