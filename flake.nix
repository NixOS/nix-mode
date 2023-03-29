{
  description = "An emacs major mode for editing Nix expressions";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
  in {
    packages = forAllSystems (system: with (import nixpkgs { inherit system; }); {
      default = let
        emacs = emacsWithPackages (epkgs: with epkgs; [
          org-contrib
          company
          mmm-mode
          magit-section
          transient
        ]);
      in stdenv.mkDerivation {
        pname = "nix-mode";
        version = "1.5.0";
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

    # checks are run in ‘make check’ right now we should probably move
    # these to its own derivation
    checks = forAllSystems (system: {
      inherit (self.packages.${system}) default;
    });
  };
}
