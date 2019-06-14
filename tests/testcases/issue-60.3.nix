let
  mozilla-overlay =
    import
      (
        builtins.fetchTarball
          https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz
      );
in mozilla-overlay
