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
(require 'json)
(eval-when-compile
  (require 'let-alist))

(defgroup nix nil
  "Nix-related customizations."
  :group 'languages)

(defcustom nix-executable "nix"
  "Nix executable location."
  :group 'nix
  :type 'string)

(defcustom nix-build-executable "nix-build"
  "Nix-build executable location."
  :group 'nix
  :type 'string)

(defcustom nix-instantiate-executable "nix-instantiate"
  "Nix-instantiate executable location."
  :group 'nix
  :type 'string)

(defcustom nix-store-executable "nix-store"
  "Nix-store executable location."
  :group 'nix
  :type 'string)

(defcustom nix-shell-executable "nix-shell"
  "Location of ‘nix-shell’ executable."
  :group 'nix
  :type 'string)

(defcustom nix-store-dir "/nix/store"
  "Nix store directory."
  :group 'nix
  :type 'directory)

(defcustom nix-state-dir "/nix/var"
  "Nix state directory."
  :group 'nix
  :type 'directory)

(defun nix-system ()
  "Get the current system tuple."
  (nix--process-string "eval"
    "--raw"
    (if (nix-is-24) "--impure" )
    (if (nix-is-24) "--expr" )
    "(builtins.currentSystem)"))

(defvar nix-version nil)
(defun nix-version ()
  "Get the version of Nix."
  (or nix-version (nix--process-string "--version")))

(defun nix-show-config ()
  "Show nix config."
  (nix--process-json "show-config" "--json"))

(defconst nix-commands
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

(defconst nix-toplevel-options
  '("-v"
    "--verbose"
    "-h"
    "--help"
    "--debug"
    "--help-config"
    "--option"
    "--version"))

(defconst nix-config-options
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

(defun nix-is-24 ()
  "Whether Nix is a version with Flakes support."
  (let ((version (nix-version)))
    (save-match-data
      (when (string-match (rx bol "nix (Nix) " (group (+ digit) (?  "." (+ digit))))
                          version)
        (version<= "2.4" (match-string 1 version))))))

(defun nix-has-flakes ()
  "Whether Nix is a version with Flakes support."
  ;; earlier versions reported as 3, now it’s just nix-2.4
  (and (nix-is-24)
       (let-alist (nix-show-config)
	 (or
	  (seq-contains-p .experimental-features.value 1)
	  (seq-contains-p .experimental-features.value "flakes")))))

;;;###autoload
(defun pcomplete/nix ()
  "Completion for the nix command."
  (if (nix-is-24)
      (let ((stdout (generate-new-buffer "nix-completions"))
            (process-environment
             (cons (format "NIX_GET_COMPLETIONS=%s" (1- (length pcomplete-args)))
                   process-environment))
            result)
        (apply 'call-process nix-executable nil (list stdout nil) nil
               (cdr pcomplete-args))
        (with-current-buffer stdout (setq result (buffer-string)))
        (kill-buffer stdout)
        (let ((lines (split-string result "\n"))
              completions)
          (dolist (val (cdr lines))
            (unless (string= val "")
              (setq completions (cons (car (split-string val "\t")) completions))))
          (dolist (val (cddr pcomplete-args))
            (pcomplete-here))
          (pcomplete-here completions nil t)))
    (progn
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
      (pcomplete-here (pcomplete-entries)))))

(defun nix--process (&rest args)
  (with-temp-buffer
    (let* ((tmpfile  (make-temp-file "nix--process-stderr"))
	 (cleaned-args (seq-filter #'stringp args))
	 (exitcode (apply #'call-process `(,nix-executable nil (t ,tmpfile) nil ,@cleaned-args )))
	 (stderr (with-temp-buffer
		   (insert-file-contents tmpfile)
		   (buffer-string))))
      (delete-file tmpfile)
      (list (buffer-string) stderr exitcode))))

(defun nix--process-string (&rest args)
  (cl-multiple-value-bind (stdout stderr exitcode) (apply #'nix--process args)
    (if (not (eq exitcode 0))
      (error stderr))
    ;; cut-off the trailing newline
    (string-trim-right stdout)))

(defun nix--process-json (&rest args)
  (json-read-from-string
    (apply #'nix--process-string args)))

(defun nix--process-lines (&rest args)
  (seq-filter (lambda (el) (not (string= "" el)))
    (split-string
      (apply #'nix--process-string args) "\n")))

(defun nix--process-json-nocheck (&rest args)
  ;; No checking of exitcode is possible here until
  ;; https://github.com/NixOS/nix/issues/2474 is resolved
  (let ((result (apply #'nix--process args)))
    (json-read-from-string (car result))))

(provide 'nix)
;;; nix.el ends here
