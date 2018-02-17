;;; nix-shell.el -- run nix-shell in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; To use this just run:

;; M-x RET nix-shell RET

;; This will give you some

;;; Code:

;; (require 'nix-mode)
(require 'term)

(defgroup nix-shell nil
  "Customizations for nix-shell"
  :group 'nix)

(defcustom nix-shell-executable "nix-shell"
  "Location of ‘nix-shell’ executable."
  :group 'nix-shell
  :type 'string)

;;;###autoload
(defun nix-shell (path attribute)
  "Run ‘nix-shell’ in a terminal.

PATH path containing Nix expressions.
ATTRIBUTE attribute name in nixpkgs to use."
  (interactive
   (list (read-from-minibuffer "Nix path: " "<nixpkgs>")
         (read-from-minibuffer "Nix attribute name: ")))
  (set-buffer (make-term "nix-shell" nix-shell-executable nil
                         path "-A" attribute))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*nix-shell*"))

;;;###autoload
(defun nix-unpack (path attribute)
  "Get source from a Nix derivation.

PATH used for base of Nix expresions.

ATTRIBUTE from PATH to get Nix expressions from."
  (interactive (list (read-string "Nix path: " "<nixpkgs>")
					 (read-string "Nix attribute name: ")))
  (async-shell-command (format "%s '%s' -A '%s' --run unpackPhase"
							   nix-shell-executable path attribute)))

(provide 'nix-shell)
;;; nix-shell.el ends here
