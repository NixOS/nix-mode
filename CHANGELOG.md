# Changelog

## 1.5.0

* Removed json-mode dependency, using js instead.
* Compatibility with Emacs 28+.
* Add nix-flake commands, based on magit-session.
* Add nix-store-show-log command.
* Other various fixes.

## 1.4.5

* Fixed completion support in nix-repl-mode for Nix 2.

## 1.4.4

### Changes

* Fix a bug causing Nix-based completions to throw an error

## 1.4.3

### Changes

* Better documentation for nix-prettify-mode
* Fixes for nix-eshell.el
* Use `NIX_GET_COMPLETIONS` if Nix 3.0 is installed
* Add flake.nix

## 1.4.2

### Changes

* Fixed build scripts to work with newer org-mode.
* Invoke smie indent function locally, instead of globally.
* Indent correctly when encountering preceding angle bracket paths.
* Require Emacs 25.
* Add json-mode dependency.

## 1.4.1

### Changes

* Made smie-setup optional. Some really large files can take a little
  bit to load. You can smie by setting nix-mode-use-smie to nil. It is
  still enabled by default.

* Tweaks to how smie works.

## 1.4.0

### Changes

* Introduced SMIE indentation mode. It is now the default. The old
  behavior can be restored by setting ‘nix-indent-function’ to
  indent-relative.

* Added the ‘nix-indent-region’ function to indent blocks of Nix
  expressions. This can be used as a dumb formatter.

* Better testing has been added to make sure we handle more cases of
  Nix indentation style.

## 1.3.0

* Added manual.

* Create CHANGELOG.md

### New files

* nix.el: customization settings for nix installation

## 1.2.1

### Changes

* fix byte-compile error

## 1.2.0

### New files

* nix-format.el: format Nix code using nixfmt
* nix-mode-mmm.el: treate multiline strings as sh-script within nix-mode
* nix-repl.el: run nix-repl within Emacs
* nix-prettify-mode.el: shorten store paths to /nix/store/…-foo-0.1
* nix-shell.el: run nix-shell within Emacs
* nix-company.el: complete Nix expressions through company

### Changes

All of these reflect nix-mode.el and what's been changed from the original nix-mode.el.

* add some simple tests for nix-mode
* handle antiquotes within Nix expressions better
* handle multiline string better
* fixes some edge cases for ''${ (escaped antiquote)
* indent Nix code based on Nix-specific rules (not just indent-relative)
* enforce Nix spacing style rules in nix-mode (2 spaces, no tabs)

### Bug fixes

This version fixes the following bugs in the original Nix version:

* fixes the issue where /* by a multiline string is interpreted as a comment
  (NixOS/nix#662)
* fixes antiquote highlighting within double quotes like x="${asdf}" (NixOS/nix#1055)
* fixes an issue in org-mode fontification of nix files (NixOS/nix#1040)
* Also, should these issues should be closable: NixOS/nix#1419, NixOS/nix#1086,
  NixOS/nix#1054

## 1.0

Original nix-mode
from [https://github.com/NixOS/nix/](https://github.com/NixOS/nix/). See that
repository for older changelog.
