# A shortened version of all-packages.nix to compare indentation to.

/* The top-level package collection of nixpkgs.
 * It is sorted by categories corresponding to the folder names
 * in the /pkgs folder. Inside the categories packages are roughly
 * sorted by alphabet, but strict sorting has been long lost due
 * to merges. Please use the full-text search of your editor. ;)
 * Hint: ### starts category names.
 */
{ lib, noSysDirs, config, overlays }:
res: pkgs: super:

with pkgs;

let
  self =
    builtins.trace ''
        It seems that you are using a patched Nixpkgs that references the self
        variable in pkgs/top-level/all-packages.nix. This variable was incorrectly
        named, so its usage needs attention. Please use pkgs for packages or super
        for functions.
      ''
      res; # Do *NOT* use res in your fork. It will be removed.

  # TODO: turn self into an error

in
{

  # A stdenv capable of building 32-bit binaries.  On x86_64-linux,
  # it uses GCC compiled with multilib support; on i686-linux, it's
  # just the plain stdenv.
  stdenv_32bit = lowPrio (if stdenv.hostPlatform.is32bit then stdenv else multiStdenv);

  stdenvNoCC = stdenv.override { cc = null; extraAttrs.noCC = true; };

  mkStdenvNoLibs = stdenv: let
    bintools = stdenv.cc.bintools.override {
      libc = null;
      noLibc = true;
    };
  in stdenv.override {
    cc = stdenv.cc.override {
      libc = null;
      noLibc = true;
      extraPackages = [];
      inherit bintools;
    };
    allowedRequisites =
      lib.mapNullable (rs: rs ++ [ bintools ]) (stdenv.allowedRequisites or null);
  };

  stdenvNoLibs = mkStdenvNoLibs stdenv;

  gccStdenvNoLibs = mkStdenvNoLibs gccStdenv;
  clangStdenvNoLibs = mkStdenvNoLibs clangStdenv;

  # For convenience, allow callers to get the path to Nixpkgs.
  path = ../..;


  ### Helper functions.
  inherit lib config overlays;

  inherit (lib) lowPrio hiPrio appendToName makeOverridable;

  # Applying this to an attribute set will cause nix-env to look
  # inside the set for derivations.
  recurseIntoAttrs = attrs: attrs // { recurseForDerivations = true; };

  # This is intended to be the reverse of recurseIntoAttrs, as it is
  # defined now it exists mainly for documentation purposes, but you
  # can also override this with recurseIntoAttrs to recurseInto all
  # the Attrs which is useful for testing massive changes. Ideally,
  # every package subset not marked with recurseIntoAttrs should be
  # marked with this.
  dontRecurseIntoAttrs = x: x;

  stringsWithDeps = lib.stringsWithDeps;

  ### Evaluating the entire Nixpkgs naively will fail, make failure fast
  AAAAAASomeThingsFailToEvaluate = throw ''
    Please be informed that this pseudo-package is not the only part of
    Nixpkgs that fails to evaluate. You should not evaluate entire Nixpkgs
    without some special measures to handle failing packages, like those taken
    by Hydra.
  '';

  tests = callPackages ../test {};

  ### Nixpkgs maintainer tools

  nix-generate-from-cpan = callPackage ../../maintainers/scripts/nix-generate-from-cpan.nix { };

  nixpkgs-lint = callPackage ../../maintainers/scripts/nixpkgs-lint.nix { };

  common-updater-scripts = callPackage ../common-updater/scripts.nix { };

  ### Push NixOS tests inside the fixed point

  nixosTests = import ../../nixos/tests/all-tests.nix {
    inherit pkgs;
    system = stdenv.hostPlatform.system;
    callTest = t: t.test;
  };

  ### BUILD SUPPORT

  autoreconfHook = makeSetupHook
    { deps = [ autoconf automake gettext libtool ]; }
    ../build-support/setup-hooks/autoreconf.sh;

  autoreconfHook264 = makeSetupHook
    { deps = [ autoconf264 automake111x gettext libtool ]; }
    ../build-support/setup-hooks/autoreconf.sh;

  autoPatchelfHook = makeSetupHook { name = "auto-patchelf-hook"; }
    ../build-support/setup-hooks/auto-patchelf.sh;

  appimageTools = callPackage ../build-support/appimage { };

  ensureNewerSourcesHook = { year }: makeSetupHook {}
    (writeScript "ensure-newer-sources-hook.sh" ''
      postUnpackHooks+=(_ensureNewerSources)
      _ensureNewerSources() {
        '${findutils}/bin/find' "$sourceRoot" \
          '!' -newermt '${year}-01-01' -exec touch -h -d '${year}-01-02' '{}' '+'
      }
    '');

  addOpenGLRunpath = callPackage ../build-support/add-opengl-runpath { };

  # Zip file format only allows times after year 1980, which makes e.g. Python wheel building fail with:
  # ValueError: ZIP does not support timestamps before 1980
  ensureNewerSourcesForZipFilesHook = ensureNewerSourcesHook { year = "1980"; };

  updateAutotoolsGnuConfigScriptsHook = makeSetupHook
    { substitutions = { gnu_config = gnu-config;}; }
    ../build-support/setup-hooks/update-autotools-gnu-config-scripts.sh;

  gogUnpackHook = makeSetupHook {
    name = "gog-unpack-hook";
    deps = [ innoextract file-rename ]; }
    ../build-support/setup-hooks/gog-unpack.sh;

  buildEnv = callPackage ../build-support/buildenv { }; # not actually a package

  buildFHSUserEnv = callPackage ../build-support/build-fhs-userenv { };

  buildMaven = callPackage ../build-support/build-maven.nix {};

  castxml = callPackage ../development/tools/castxml { };

  cmark = callPackage ../development/libraries/cmark { };

  corgi = callPackage ../development/tools/corgi { };

  dhallToNix = callPackage ../build-support/dhall-to-nix.nix {
    inherit dhall-nix;
  };

  deadcode = callPackage ../development/tools/deadcode { };

  proto-contrib = callPackage ../development/tools/proto-contrib {};

  demoit = callPackage ../servers/demoit { };

  diffPlugins = (callPackage ../build-support/plugins.nix {}).diffPlugins;

  dieHook = makeSetupHook {} ../build-support/setup-hooks/die.sh;

  archiver = callPackage ../applications/misc/archiver { };

  digitalbitbox = libsForQt5.callPackage ../applications/misc/digitalbitbox { };

  dockerTools = callPackage ../build-support/docker { };

  nix-prefetch-docker = callPackage ../build-support/docker/nix-prefetch-docker.nix { };

  docker-compose = python3Packages.callPackage ../applications/virtualization/docker-compose {};

  docker-ls = callPackage ../tools/misc/docker-ls { };

  docker-sync = callPackage ../tools/misc/docker-sync { };

  docui = callPackage ../tools/misc/docui { };

  dotfiles = callPackage ../applications/misc/dotfiles { };

  dotnetenv = callPackage ../build-support/dotnetenv {
    dotnetfx = dotnetfx40;
  };

  dotnetbuildhelpers = callPackage ../build-support/dotnetbuildhelpers { };

  dotnet-sdk = callPackage ../development/compilers/dotnet/sdk { };

  dispad = callPackage ../tools/X11/dispad { };

  dump1090 = callPackage ../applications/radio/dump1090 { };

  ebook2cw = callPackage ../applications/radio/ebook2cw { };

  etBook = callPackage ../data/fonts/et-book { };

  fetchbower = callPackage ../build-support/fetchbower {
    inherit (nodePackages) bower2nix;
  };

  fetchbzr = callPackage ../build-support/fetchbzr { };

  fetchcvs = callPackage ../build-support/fetchcvs { };

  fetchdarcs = callPackage ../build-support/fetchdarcs { };

  fetchdocker = callPackage ../build-support/fetchdocker { };

  fetchDockerConfig = callPackage ../build-support/fetchdocker/fetchDockerConfig.nix { };

  fetchDockerLayer = callPackage ../build-support/fetchdocker/fetchDockerLayer.nix { };

  fetchfossil = callPackage ../build-support/fetchfossil { };

  fetchgit = callPackage ../build-support/fetchgit {
    git = buildPackages.gitMinimal;
    cacert = buildPackages.cacert;
  };

  fetchgitPrivate = callPackage ../build-support/fetchgit/private.nix { };

  fetchgitLocal = callPackage ../build-support/fetchgitlocal { };

  fetchmtn = callPackage ../build-support/fetchmtn (config.fetchmtn or {});

  fetchMavenArtifact = callPackage ../build-support/fetchmavenartifact { };

  prefer-remote-fetch = import ../build-support/prefer-remote-fetch;

  global-platform-pro = callPackage ../development/tools/global-platform-pro/default.nix { };

  graph-easy = callPackage ../tools/graphics/graph-easy { };

  packer = callPackage ../development/tools/packer { };

  pet = callPackage ../development/tools/pet { };

  mod = callPackage ../development/tools/mod { };

  broadlink-cli = callPackage ../tools/misc/broadlink-cli {};

  mht2htm = callPackage ../tools/misc/mht2htm { };

  fetchpatch = callPackage ../build-support/fetchpatch { };

  fetchs3 = callPackage ../build-support/fetchs3 { };

  fetchsvn = callPackage ../build-support/fetchsvn { };

  fetchsvnrevision = import ../build-support/fetchsvnrevision runCommand subversion;

  fetchsvnssh = callPackage ../build-support/fetchsvnssh { };

  fetchhg = callPackage ../build-support/fetchhg { };

  # `fetchurl' downloads a file from the network.
  fetchurl = import ../build-support/fetchurl {
    inherit lib stdenvNoCC;
    curl = buildPackages.curl.override rec {
      # break dependency cycles
      fetchurl = stdenv.fetchurlBoot;
      zlib = buildPackages.zlib.override { fetchurl = stdenv.fetchurlBoot; };
      pkgconfig = buildPackages.pkgconfig.override { fetchurl = stdenv.fetchurlBoot; };
      perl = buildPackages.perl.override { fetchurl = stdenv.fetchurlBoot; };
      openssl = buildPackages.openssl.override {
        fetchurl = stdenv.fetchurlBoot;
        inherit perl;
        buildPackages = { inherit perl; };
      };
      libssh2 = buildPackages.libssh2.override {
        fetchurl = stdenv.fetchurlBoot;
        inherit zlib openssl;
      };
      # On darwin, libkrb5 needs bootstrap_cmds which would require
      # converting many packages to fetchurl_boot to avoid evaluation cycles.
      gssSupport = !stdenv.isDarwin && !stdenv.hostPlatform.isWindows;
      libkrb5 = buildPackages.libkrb5.override {
        fetchurl = stdenv.fetchurlBoot;
        inherit pkgconfig perl openssl;
        keyutils = buildPackages.keyutils.override { fetchurl = stdenv.fetchurlBoot; };
      };
      nghttp2 = buildPackages.nghttp2.override {
        fetchurl = stdenv.fetchurlBoot;
        inherit zlib pkgconfig openssl;
        c-ares = buildPackages.c-ares.override { fetchurl = stdenv.fetchurlBoot; };
        libev = buildPackages.libev.override { fetchurl = stdenv.fetchurlBoot; };
      };
    };
  };

  fetchRepoProject = callPackage ../build-support/fetchrepoproject { };

  fetchipfs = import ../build-support/fetchipfs {
    inherit curl stdenv;
  };

  fetchzip = callPackage ../build-support/fetchzip { };

  fetchCrate = callPackage ../build-support/rust/fetchcrate.nix { };

  fetchFromGitHub = callPackage ../build-support/fetchgithub {};

  fetchFromBitbucket = callPackage ../build-support/fetchbitbucket {};

  fetchFromSavannah = callPackage ../build-support/fetchsavannah {};

  fetchFromGitLab = callPackage ../build-support/fetchgitlab {};

  fetchFromRepoOrCz = callPackage ../build-support/fetchrepoorcz {};

  fetchNuGet = callPackage ../build-support/fetchnuget { };
  buildDotnetPackage = callPackage ../build-support/build-dotnet-package { };

  fetchgx = callPackage ../build-support/fetchgx { };

  resolveMirrorURLs = {url}: fetchurl {
    showURLs = true;
    inherit url;
  };

  ld-is-cc-hook = makeSetupHook { name = "ld-is-cc-hook"; }
    ../build-support/setup-hooks/ld-is-cc-hook.sh;

  libredirect = callPackage ../build-support/libredirect { };

  madonctl = callPackage ../applications/misc/madonctl { };

  makeDesktopItem = callPackage ../build-support/make-desktopitem { };

  makeAutostartItem = callPackage ../build-support/make-startupitem { };

  makeInitrd = callPackage ../build-support/kernel/make-initrd.nix; # Args intentionally left out

  makeWrapper = makeSetupHook { deps = [ dieHook ]; substitutions = { shell = pkgs.runtimeShell; }; }
    ../build-support/setup-hooks/make-wrapper.sh;

  makeModulesClosure = { kernel, firmware, rootModules, allowMissing ? false }:
    callPackage ../build-support/kernel/modules-closure.nix {
      inherit kernel firmware rootModules allowMissing;
    };

  mkShell = callPackage ../build-support/mkshell { };

  nixBufferBuilders = import ../build-support/emacs/buffer.nix { inherit (pkgs) lib writeText; inherit (emacsPackagesNg) inherit-local; };

  nix-gitignore = callPackage ../build-support/nix-gitignore { };

  pathsFromGraph = ../build-support/kernel/paths-from-graph.pl;

  pruneLibtoolFiles = makeSetupHook { name = "prune-libtool-files"; }
    ../build-support/setup-hooks/prune-libtool-files.sh;

  closureInfo = callPackage ../build-support/closure-info.nix { };

  setupSystemdUnits = callPackage ../build-support/setup-systemd-units.nix { };

  singularity-tools = callPackage ../build-support/singularity-tools { };

  srcOnly = args: callPackage ../build-support/src-only args;

  substituteAll = callPackage ../build-support/substitute/substitute-all.nix { };

  substituteAllFiles = callPackage ../build-support/substitute-files/substitute-all-files.nix { };

  replaceDependency = callPackage ../build-support/replace-dependency.nix { };

  nukeReferences = callPackage ../build-support/nuke-references { };

  referencesByPopularity = callPackage ../build-support/references-by-popularity { };

  removeReferencesTo = callPackage ../build-support/remove-references-to { };

  vmTools = callPackage ../build-support/vm { };

  releaseTools = callPackage ../build-support/release { };

  inherit (lib.systems) platforms;

  setJavaClassPath = makeSetupHook { } ../build-support/setup-hooks/set-java-classpath.sh;

  fixDarwinDylibNames = makeSetupHook { } ../build-support/setup-hooks/fix-darwin-dylib-names.sh;

  keepBuildTree = makeSetupHook { } ../build-support/setup-hooks/keep-build-tree.sh;

  enableGCOVInstrumentation = makeSetupHook { } ../build-support/setup-hooks/enable-coverage-instrumentation.sh;

  makeGCOVReport = makeSetupHook
    { deps = [ pkgs.lcov pkgs.enableGCOVInstrumentation ]; }
    ../build-support/setup-hooks/make-coverage-analysis-report.sh;

  # intended to be used like nix-build -E 'with import <nixpkgs> {}; enableDebugging fooPackage'
  enableDebugging = pkg: pkg.override { stdenv = stdenvAdapters.keepDebugInfo pkg.stdenv; };

  findXMLCatalogs = makeSetupHook { } ../build-support/setup-hooks/find-xml-catalogs.sh;

  wrapGAppsHook = makeSetupHook {
    deps = lib.optional (!stdenv.isDarwin) gnome3.dconf.lib ++ [ gtk3 librsvg makeWrapper ];
  } ../build-support/setup-hooks/wrap-gapps-hook.sh;

  separateDebugInfo = makeSetupHook { } ../build-support/setup-hooks/separate-debug-info.sh;

  setupDebugInfoDirs = makeSetupHook { } ../build-support/setup-hooks/setup-debug-info-dirs.sh;

  useOldCXXAbi = makeSetupHook { } ../build-support/setup-hooks/use-old-cxx-abi.sh;

  ical2org = callPackage ../tools/misc/ical2org {};

  iconConvTools = callPackage ../build-support/icon-conv-tools {};

  #package writers
  writers = callPackage ../build-support/writers {};

  ### TOOLS

  _0x0 = callPackage ../tools/misc/0x0 { };

  _1password = callPackage ../applications/misc/1password { };

  _9pfs = callPackage ../tools/filesystems/9pfs { };

  a2ps = callPackage ../tools/text/a2ps { };

  abcm2ps = callPackage ../tools/audio/abcm2ps { };

  abcmidi = callPackage ../tools/audio/abcmidi { };

  abduco = callPackage ../tools/misc/abduco { };

  acct = callPackage ../tools/system/acct { };

  accuraterip-checksum = callPackage ../tools/audio/accuraterip-checksum { };

  acme-sh = callPackage ../tools/admin/acme.sh { };

  acoustidFingerprinter = callPackage ../tools/audio/acoustid-fingerprinter {
    ffmpeg = ffmpeg_2;
  };

  acpica-tools = callPackage ../tools/system/acpica-tools { };

  actdiag = with python3.pkgs; toPythonApplication actdiag;

  actkbd = callPackage ../tools/system/actkbd { };

  adafruit-ampy = callPackage ../tools/misc/adafruit-ampy { };

  adlplug = callPackage ../applications/audio/adlplug { };

  opnplug = callPackage ../applications/audio/adlplug {
    adlplugChip = "-DADLplug_CHIP=OPN2";
    pname = "OPNplug";
  };

  advancecomp = callPackage ../tools/compression/advancecomp {};

  aefs = callPackage ../tools/filesystems/aefs { };

  aegisub = callPackage ../applications/video/aegisub ({
    wxGTK = wxGTK30;
  } // (config.aegisub or {}));

  aerc = callPackage ../applications/networking/mailreaders/aerc { };

  aerospike = callPackage ../servers/nosql/aerospike { };

  aespipe = callPackage ../tools/security/aespipe { };

  aescrypt = callPackage ../tools/misc/aescrypt { };

  acme-client = callPackage ../tools/networking/acme-client { inherit (darwin) apple_sdk; };

  amass = callPackage ../tools/networking/amass { };

  afew = callPackage ../applications/networking/mailreaders/afew { pythonPackages = python3Packages; };

  afio = callPackage ../tools/archivers/afio { };

  afl = callPackage ../tools/security/afl {
    stdenv = clangStdenv;
  };

  libdislocator = callPackage ../tools/security/afl/libdislocator.nix { };

  afpfs-ng = callPackage ../tools/filesystems/afpfs-ng { };

  agrep = callPackage ../tools/text/agrep { };

  aha = callPackage ../tools/text/aha { };

  ahcpd = callPackage ../tools/networking/ahcpd { };

  aide = callPackage ../tools/security/aide { };

  aircrack-ng = callPackage ../tools/networking/aircrack-ng { };

  airfield = callPackage ../tools/networking/airfield { };

  airsonic = callPackage ../servers/misc/airsonic { };

  airspy = callPackage ../applications/radio/airspy { };

  airtame = callPackage ../applications/misc/airtame { };

  aj-snapshot  = callPackage ../applications/audio/aj-snapshot { };

  albert = libsForQt5.callPackage ../applications/misc/albert {};

  alacritty = callPackage ../applications/misc/alacritty {
    inherit (xorg) libXcursor libXxf86vm libXi;
    inherit (darwin) cf-private;
    inherit (darwin.apple_sdk.frameworks) AppKit CoreFoundation CoreGraphics CoreServices CoreText Foundation OpenGL;
  };

  aldo = callPackage ../applications/radio/aldo { };

  almanah = callPackage ../applications/misc/almanah { };

  amazon-ecs-cli = callPackage ../tools/virtualization/amazon-ecs-cli { };

  amazon-glacier-cmd-interface = callPackage ../tools/backup/amazon-glacier-cmd-interface { };

  amber = callPackage ../tools/text/amber {
    inherit (darwin.apple_sdk.frameworks) Security;
  };

  ammonite = callPackage ../development/tools/ammonite {};

  amtterm = callPackage ../tools/system/amtterm {};

  analog = callPackage ../tools/admin/analog {};

  ansifilter = callPackage ../tools/text/ansifilter {};

  apktool = callPackage ../development/tools/apktool {
    inherit (androidenv.androidPkgs_9_0) build-tools;
  };

}
