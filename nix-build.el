;;; nix-build.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-search)

;;;###autoload
(defun nix-build (&optional file attr)
  "Run nix-build in a compilation buffer.
FILE the file to parse.
ATTR the attribute to build."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (setq compile-command (format "%s %s -A '%s'" nix-build-executable
				file attr))
  (setq-default compilation-directory default-directory)
  (compilation-start compile-command nil
		     (apply-partially (lambda (attr _)
					(format "*nix-build*<%s>" attr))
				      attr)))

(provide 'nix-build)
;;; nix-build.el ends here
