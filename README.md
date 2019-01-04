# nix-mode

[![MELPA](https://melpa.org/packages/nix-mode-badge.svg)](https://melpa.org/#/nix-mode)
[![MELPA Stable](https://stable.melpa.org/packages/nix-mode-badge.svg)](https://stable.melpa.org/#/nix-mode)
[![Build Status](https://travis-ci.com/NixOS/nix-mode.svg?branch=master)](https://travis-ci.com/NixOS/nix-mode)

An emacs major mode for editing nix expressions.

## Submodes

A quick list of what is provided.

### nix.el

nix.el contains some miscellaneous tools for Nix developers.
Interactive functions include:

- nix-unpack - unpack source of a Nix attribute.

  To use this just type:

  M-x nix-unpack<RET>

  Followed by your Nix path & attribute path.

- nix-build - build a Nix derviation.

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

### nix-repl.el

nix-repl.el has two purposes.

First, it provides an interface for completion, used by nix-company.el.

Second, it provides an interactive function to open an repl. You can
open this with:

M-x nix-repl<RET>

## Origins

This repository is based off of the nix-mode.el file originally located in
the [Nix repository](https://github.com/NixOS/nix)
at
[misc/emacs/nix-mode.el](https://github.com/NixOS/nix/blob/master/misc/emacs/nix-mode.el).
Please see [the CHANGELOG file](https://github.com/NixOS/nix-mode/blob/master/CHANGELOG.md) for a list of changes.

## Other Emacs packages

@shlevy has an excellent package for integrating nix-shell into emacs. It is available at [shlevy/nix-buffer](https://github.com/shlevy/nix-buffer). 

@travisbhartwell also has some package dealing with Nix. They are available at [travisbhartwell/nix-emacs](https://github.com/travisbhartwell/nix-emacs).
