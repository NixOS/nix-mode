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

(require 'nix)
(require 'nix-search)

(defun nix-build (&optional attr)
  "Run nix-build in a compilation buffer."
  (interactive (list (nix-search-read-attr "./.")))
  (setq compile-command (format "%s -A '%s'" nix-build-executable attr))
  (setq-default compilation-directory default-directory)
  (compilation-start compile-command nil
		     (apply-partially (lambda (attr _)
					(format "*nix-build*<%s>" attr))
				      attr)))

(provide 'nix-build)
;;; nix-build.el ends here
