{ pkgs ? import <nixpkgs> {}}:
let
  inherit (pkgs) emacsWithPackages stdenvNoCC texinfo;
  emacs = emacsWithPackages (epkgs: with epkgs; [
    org-plus-contrib
    company
    json-mode
    mmm-mode
  ]);
in stdenvNoCC.mkDerivation {
  name = "nix-mode";
  src = ./.;
  nativeBuildInputs = [ emacs texinfo ];
  makeFlags = [ "PREFIX=$(out)" ];
  shellHook = ''
    echo Run make run to get vanilla emacs with nix-mode loaded.
  '';
  doCheck = true;
}
