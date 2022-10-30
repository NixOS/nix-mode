# nix-mode

[![MELPA](https://melpa.org/packages/nix-mode-badge.svg)](https://melpa.org/#/nix-mode)
[![MELPA Stable](https://stable.melpa.org/packages/nix-mode-badge.svg)](https://stable.melpa.org/#/nix-mode)
[![Build Status](https://travis-ci.com/NixOS/nix-mode.svg?branch=master)](https://travis-ci.com/NixOS/nix-mode)

An Emacs major mode for editing Nix expressions. There is also a
manual available at [nix-mode.org](./nix-mode.org).

## Submodes

A quick list of what is provided.

### nix-mode.el

This is the main usage of nix-mode. This provides basic handling of
.nix files. Syntax highlighting and indentation support using SMIE are
provided. nix-mode can be used with the following snippet:

~~~
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
~~~

or with use-package:

~~~
(use-package nix-mode
  :mode "\\.nix\\'")
~~~

### nix.el

nix.el contains some miscellaneous tools for Nix developers.
Interactive functions include:

- nix-unpack - unpack source of a Nix attribute.

  To use this just type:

  M-x nix-unpack<RET>

  Followed by your Nix path & attribute path.

- nix-build - build a Nix derivation.

  This is meant to work similarly to M-x compile. It will use your
  current directory & build it if there is a default.nix there.

nix.el also provides some basic functions for interfacing with Nix.
Some variables are provided to point to the Nix binaries that can be
used in Lisp code:

- nix-executable
- nix-build-executable
- nix-instantiate-executable
- nix-store-executable
- nix-shell-executable

Other useful functions for Lisp scripts are provided:

- nix-system - Get the current system, detected by Nix

### nix-flake.el

nix-flake.el provides support for flake commands.
These commands are experimental as of Nix 2.4.

It uses transient.el to provide a magit-like interface.
To run a command on the current flake, type:

M-x nix-flake<RET>

You can also initialize a flake from a template:

M-x nix-flake-init<RET>

### nix-repl.el

nix-repl.el has two purposes.

First, it provides an interface for completion, used by nix-company.el.

Second, it provides an interactive function to open a repl. You can
open this with:

M-x nix-repl<RET>

### nix-store.el

This file provides a command `M-x nix-store-show-path`. The command displays an
overview of a store path. The information it shows is the realisation status,
the hash and the size of the store path. Also it shows lists of derivers,
references, referrers and requisites of the respective path.

You can change the order in which that information is shown. See the
documentation of the function `nix-store-show-path` for more information.

When viewing a store buffer, the command `M-x nix-store-show-log`
opens a local log file associated with a derivation.

### nix-prettify-mode.el

When nix-prettify-mode is enabled, hash-parts of the Nix store file names are
prettified, i.e. displayed as `nix-prettify-char` character (`…` by default.).

This is based on a similar mode for Guix: [Prettify Mode (Emacs-Guix Reference Manual)](https://emacs-guix.gitlab.io/website/manual/0.4/html_node/Prettify-Mode.html).

## Origins

This repository is based off of the nix-mode.el file originally located in
the [Nix repository](https://github.com/NixOS/nix)
at
[misc/emacs/nix-mode.el](https://github.com/NixOS/nix/blob/master/misc/emacs/nix-mode.el).
Please see [the CHANGELOG file](https://github.com/NixOS/nix-mode/blob/master/CHANGELOG.md) for a list of changes.

## Other Emacs packages

@shlevy has an excellent package for integrating nix-shell into emacs. It is available at [shlevy/nix-buffer](https://github.com/shlevy/nix-buffer). 

@travisbhartwell also has some package dealing with Nix. They are available at [travisbhartwell/nix-emacs](https://github.com/travisbhartwell/nix-emacs).
