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

;;;###autoload
(defun pcomplete/nix ()
  "Completion for the nix command."
  (while (pcomplete-match "^-" 0)
    (pcomplete-here nix-toplevel-options)
    (when (string= "--option"
                   (nth (1- pcomplete-index) pcomplete-args))
      (pcomplete-here nix-config-options)
      (pcomplete-here)))
  (pcomplete-here nix-commands))

;;;###autoload
(defun nix-build (&optional attr dir)
  "Run nix-build.
ATTR is the attribute to build.
DIR is the directory containing the Nix default.nix expression."
  (interactive)
  (unless dir (setq dir default-directory))
  (if attr
      (async-shell-command (format "%s %s -A %s" nix-build-executable dir attr))
    (async-shell-command (format "%s %s" nix-build-executable dir))))

;;;###autoload
(defun nix-unpack (path attribute)
  "Get source from a Nix derivation.

PATH used for base of Nix expresions.

ATTRIBUTE from PATH to get Nix expressions from."
  (interactive (list (read-string "Nix path: " "<nixpkgs>")
		     (read-string "Nix attribute name: ")))
  (async-shell-command (format "%s '%s' -A '%s' --run unpackPhase"
			       nix-shell-executable
			       path attribute)))

;;;###autoload
(define-minor-mode global-nix-mode
  "Minor mode to enable Nix enhancements."
  :require 'nix
  :global t
  (if global-nix-mode
      (progn
        (add-to-list 'interpreter-mode-alist '("nix-shell" . nix-shebang-mode))
        (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
        ;; (add-to-list 'auto-mode-alist '("\\.drv\\'" . nix-drv-mode))
        (add-hook 'after-change-major-mode-hook 'nix-shell))
    (progn
      (setq interpreter-mode-alist (remove '("nix-shell" . nix-shebang-mode)
                                           interpreter-mode-alist))
      (setq auto-mode-alist
            (remove '("\\.drv\\'" . nix-drv-mode)
                    (remove '("\\.nix\\'" . nix-mode) auto-mode-alist)))
      (remove-hook 'after-change-major-mode-hook 'nix-shell))))

(provide 'nix)
;;; nix.el ends here
