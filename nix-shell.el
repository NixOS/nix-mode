;;; nix-shell.el -- run nix-shell in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/matthewbauer/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'term)

(defcustom nix-shell-executable "nix-shell"
  "Location of nix-shell executable."
  :group 'nix)

;;;###autoload
(defun nix-shell (attribute)
  "Run nix-shell in a terminal.

ATTRIBUTE attribute name in nixpkgs to use."
  (interactive (list (read-from-minibuffer "Attribute name: ")))
  (set-buffer (make-term "nix-shell" nix-shell-executable nil
                         "<nixpkgs>" "-A" attribute))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*nix-shell*"))

(provide 'nix-shell)
;;; nix-shell.el ends here
