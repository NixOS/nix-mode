;;; nix-sandbox.el --- Utility functions to work with nix-shell sandboxes

;; Copyright (C) 2015 Sven Keidel

;; Author: Sven Keidel <svenkeidel@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((dash "2.12.1") (s "1.10.0"))
;; Homepage: https://github.com/travisbhartwell/nix-emacs

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for working with nix-shell sandboxes

;;; Code:

(require 'dash)
(require 's)

(defgroup nix nil
  "customizations for nix"
  :prefix "nix-"
  :group 'external)

(defcustom nix-nixpkgs-path nil
  "Absolute path to a nixpkgs directory.

Can be customized to select a nix-channel
e.g. /home/user/.nix-defexpr/channels/unstable/nixpkgs"
  :group 'nix
  :type '(choice (const :tag "No channel" nil)
                 (directory "Custom path to a nixpkgs distribution")))

(defun nix-create-sandbox-rc (sandbox)
  "Create a new rc file containing the environment for the given SANDBOX."
  (let ((env-str (shell-command-to-string
                  (concat "nix-shell "
                          (or (and nix-nixpkgs-path (concat "-I nixpkgs=" nix-nixpkgs-path))
                              "")
                          " --run 'declare +x shellHook; declare -x; declare -xf' "
                          (shell-quote-argument sandbox)
                          " 2> /dev/null")))
        (tmp-file (make-temp-file "nix-sandbox-rc-")))
    (write-region env-str nil tmp-file 'append)
    tmp-file))

(defvar nix-sandbox-rc-map (make-hash-table :test 'equal
                                            :size 4))

(defun nix-sandbox-rc (sandbox)
  "Return the rc file for the given SANDBOX or create one."
  (or (gethash sandbox nix-sandbox-rc-map)
      (puthash sandbox (nix-create-sandbox-rc sandbox) nix-sandbox-rc-map)))

;;;###autoload
(defun nix-shell-command (sandbox &rest args)
  "Assemble a command from ARGS that can be executed in the specified SANDBOX."
  (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc sandbox) (s-join " " args))))

(defun nix-shell-string (sandbox &rest args)
  "Assemble a command string from ARGS that can be executed in the specifed SANDBOX."
  (combine-and-quote-strings
   (apply 'nix-shell-command sandbox args)))

;;;###autoload
(defun nix-compile (sandbox &rest command)
  "Compile a program using the given COMMAND in SANDBOX."
  (interactive "Dsandbox: \nMcommand: ")
  (compile (apply 'nix-shell-string sandbox command)))

;;;###autoload
(defun nix-shell (sandbox &rest command)
  "Run a COMMAND in the given SANDBOX and return the output."
  (shell-command-to-string (apply 'nix-shell-string sandbox command)))

(defvar nix-exec-path-map (make-hash-table :test 'equal
                                           :size 4))

;;;###autoload
(defun nix-exec-path (sandbox)
  "Return the `exec-path' of the given SANDBOX."

  (or (gethash sandbox nix-exec-path-map)
      (puthash sandbox
               (split-string (nix-shell sandbox "printenv" "PATH") ":")
               nix-exec-path-map)))

;;;###autoload
(defun nix-executable-find (sandbox executable)
  "Search for an EXECUTABLE in the given SANDBOX."
  (let ((exec-path (nix-exec-path sandbox)))
    (and exec-path (executable-find executable))))

;;;###autoload
(defun nix-find-sandbox (path)
  "Search for a sandbox starting at PATH traversing upwards the directory tree.
If the directory contains a `shell.nix' file, the path to this
file is returned.  Otherwise if the directory contains a
`default.nix' file, the parent directory is returned."
  (and (file-exists-p path)
       (let* ((map-nil (lambda (f x) (if x (funcall f x) nil)))
              (sandbox-directory
               (funcall map-nil 'expand-file-name
                        (locate-dominating-file path
                                                '(lambda (dir) (directory-files dir t ".*\.nix$")))))
              (shell-nix (and sandbox-directory (concat sandbox-directory "shell.nix"))))
         (if (and sandbox-directory (file-exists-p shell-nix))
             shell-nix
           sandbox-directory))))

;;;###autoload
(defun nix-current-sandbox ()
  "Return the path of the sandbox that is closest to the current working directory."
  (nix-find-sandbox default-directory))

(defun nix-clear-caches ()
  "Clear cached information for all sandboxes."
  (interactive)
  (clrhash nix-sandbox-rc-map)
  (clrhash nix-exec-path-map))

(provide 'nix-sandbox)

;;; nix-sandbox.el ends here
