;;; nix.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; To use this just run:

;; M-x RET nix-shell RET

;; This will give you some

;;; Code:

(require 'pcomplete)

(defgroup nix nil
  "Nix-related customizations"
  :group 'languages)

(defcustom nix-executable (executable-find "nix")
  "Nix executable location."
  :group 'nix
  :type 'string)

(defcustom nix-build-executable (executable-find "nix-build")
  "Nix-build executable location."
  :group 'nix
  :type 'string)

(defcustom nix-instantiate-executable (executable-find "nix-instantiate")
  "Nix executable location."
  :group 'nix
  :type 'string)

(defcustom nix-store-executable (executable-find "nix-store")
  "Nix executable location."
  :group 'nix
  :type 'string)

(defcustom nix-shell-executable (executable-find "nix-shell")
  "Location of ‘nix-shell’ executable."
  :group 'nix
  :type 'string)

(defcustom nix-store-dir "/nix/store"
  "Nix store directory."
  :group 'nix
  :type 'string)

(defcustom nix-state-dir "/nix/var"
  "Nix store directory."
  :group 'nix
  :type 'string)

(defun nix-system ()
  "Get the current system tuple."
  (let ((stdout (generate-new-buffer "nix eval"))
        result)
    (call-process nix-executable nil (list stdout nil) nil
		  "eval" "--raw" "(builtins.currentSystem)")
    (with-current-buffer stdout (setq result (buffer-string)))
    (kill-buffer stdout)
    result))

(defvar nix-commands
  '("add-to-store"
    "build"
    "cat-nar"
    "cat-store"
    "copy"
    "copy-sigs"
    "dump-path"
    "edit"
    "eval"
    "hash-file"
    "hash-path"
    "log"
    "ls-nar"
    "ls-store"
    "optimise-store"
    "path-info"
    "ping-store"
    "repl"
    "run"
    "search"
    "show-config"
    "show-derivation"
    "sign-paths"
    "to-base16"
    "to-base32"
    "to-base64"
    "upgrade-nix"
    "verify"
    "why-depends"))

(defvar nix-toplevel-options
  '("-v"
    "--verbose"
    "-h"
    "--help"
    "--debug"
    "--help-config"
    "--option"
    "--version"))

(defvar nix-config-options
  '("allowed-uris"
    "allow-import-from-derivation"
    "allow-new-priveleges"
    "allowed-users"
    "auto-optimise-store"
    "builders"
    "builders-use-substitutes"
    "build-users-group"
    "compress-build-log"
    "connect-timeout"
    "cores"
    "extra-sandbox-paths"
    "extra-substituters"
    "fallback"
    "fsync-metadata"
    "hashed-mirrors"
    "http-connections"
    "keep-build-log"
    "keep-derivations"
    "keep-env-derivations"
    "keep-outputs"
    "max-build-log-size"
    "max-jobs"
    "max-silent-time"
    "netrc-file"
    "plugin-files"
    "pre-build-hook"
    "repeat"
    "require-sigs"
    "restrict-eval"
    "sandbox"
    "sandbox-dev-shm-size"
    "sandbox-paths"
    "secret-key-files"
    "show-trace"
    "substitute"
    "substituters"
    "system"
    "timeout"
    "trusted-public-keys"
    "trusted-subtituters"
    "trusted-users"))

(defun nix--pcomplete-flags (options)
  "Complete flags to the Nix command.
OPTIONS a list of options to accept."
  (while (pcomplete-match "^-" 0)
    (pcomplete-here options)
    (let ((last-arg (nth (1- pcomplete-index) pcomplete-args)))
      (cond
       ((string= "--option" last-arg)
        (pcomplete-here nix-config-options)
        (pcomplete-here))
       ((or (string= "-f" last-arg) (string= "--file" last-arg))
        (pcomplete-here (pcomplete-entries nil 'file-exists-p)))
       ((or (string= "--arg" last-arg) (string= "--argstr" last-arg))
        (pcomplete-here)
        (pcomplete-here))
       ((or (string= "-I" last-arg) (string= "--include" last-arg))
        (pcomplete-here (pcomplete-entries nil 'file-exists-p)))
       ((or (string= "-k" last-arg) (string= "--keep" last-arg))
        (pcomplete-here))
       ((or (string= "-u" last-arg) (string= "--unset" last-arg))
        (pcomplete-here))
       ((or (string= "-s" last-arg) (string= "--substituter" last-arg))
        (pcomplete-here))))))

;;;###autoload
(defun pcomplete/nix ()
  "Completion for the nix command."
  (nix--pcomplete-flags nix-toplevel-options)
  (pcomplete-here nix-commands)
  (pcase (nth (1- pcomplete-index) pcomplete-args)
    ("run"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr" "-c" "--command"
                                     "-f" "--file" "-i" "-I" "--include"
                                     "-k" "--keep" "-u" "--unset"))))
    ("build"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr" "--dry-run"
                                     "-f" "--file" "-I" "--include"
                                     "--no-link" "-o" "--out-link"))))
    ("add-to-store"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--dry-run" "-n" "--name"))))
    ("copy"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--all" "--arg" "--argstr"
                                     "-f" "--file" "--from"
                                     "-I" "--include" "--no-check-sigs"
                                     "--no-recursive" "-s" "--substitute"
                                     "--to"))))
    ("copy-sigs"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--all" "--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"
                                     "-r" "--recursive" "-s" "--substituter"))))
    ("dump-path"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"))))
    ("edit"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"))))
    ("eval"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"
                                     "--json" "--raw"))))
    ("hash-file"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--base16" "--base32"
                                     "--base64" "--type"))))
    ("hash-path"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--base16" "--base32"
                                     "--base64" "--type"))))
    ("log"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"
                                     "--json" "--raw"))))
    ("ls-nar"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("-d" "--directory"
                                     "--json" "-l" "--long"
                                     "-R" "--recursive"))))
    ("ls-store"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("-d" "--directory"
                                     "--json" "-l" "--long"
                                     "-R" "--recursive"))))
    ("repl"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr"
                                     "-I" "--include"))))
    ("search"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr"
                                     "-f" "--file"
                                     "-I" "--include"
                                     "--json" "--no-cache"
                                     "-u" "--update-cache"))))
    ("show-config"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--json"))))
    ("show-derivation"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--arg" "--argstr"
                                     "-f" "--file"
                                     "-I" "--include"
                                     "-r" "--recursive"))))
    ("sign-paths"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--all" "--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"
                                     "-k" "--key-file" "-r" "--recursive"))))
    ("upgrade-nix"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("-p" "--profile"))))
    ("verify"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("--all" "--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"
                                     "--no-contents" "--no-trust"
                                     "-r" "--recursive" "-n" "--sigs-needed"
                                     "-s" "--substuter"))))
    ("why-depends"
     (nix--pcomplete-flags
      (append nix-toplevel-options '("-a" "--all" "--arg" "--argstr"
                                     "-f" "--file" "-I" "--include"))))
    (_ (nix--pcomplete-flags nix-toplevel-options)))
  (pcomplete-here (pcomplete-entries)))

(provide 'nix)
;;; nix.el ends here
