{
  description = "An emacs major mode for editing Nix expressions";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
  in {
    packages = forAllSystems (system: with (import nixpkgs { inherit system; }); {
      nix-mode = let
        emacs = emacsWithPackages (epkgs: with epkgs; [
          org-plus-contrib
          company
          mmm-mode
          f
          magit-section
          transient
        ]);
      in stdenvNoCC.mkDerivation {
        pname = "nix-mode";
        version = "1.4.5";
        src = self;
        nativeBuildInputs = [ emacs texinfo git ];
        makeFlags = [ "PREFIX=$(out)" ];
        shellHook = ''
          echo Run make run to get vanilla emacs with nix-mode loaded.
        '';
        doCheck = true;
        meta.description = "An emacs major mode for editing Nix expressions";
      };
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.nix-mode);

    # checks are run in ‘make check’ right now we should probably move
    # these to its own derivation
    checks = forAllSystems (system: {
      inherit (self.packages.${system}) nix-mode;
    });
  };
}
