;;; nix-drv-mode.el --- Major mode for viewing .drv files -*- lexical-binding: t -*-

;; Maintainer: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, languages, tools, unix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A major mode for viewing Nix derivations (.drv files). See the Nix
;; manual for more information available at
;; https://nixos.org/nix/manual/.

;;; Code:

(require 'js)
(require 'nix)

;;;###autoload
(define-derived-mode nix-drv-mode js-mode "Nix-Derivation"
  "Pretty print Nix’s .drv files."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (shell-command-to-string
             (format "%s show-derivation \"%s\""
		     nix-executable
		     (buffer-file-name))))
    (set-buffer-modified-p nil)
    (read-only-mode 1))

  (add-hook 'change-major-mode-hook #'nix-drv-mode-dejsonify-buffer nil t))

(defun nix-drv-mode-dejsonify-buffer ()
  "Restore nix-drv-mode when switching to another mode."

  (remove-hook 'change-major-mode-hook #'nix-drv-mode-dejsonify-buffer t)

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-buffer-modified-p nil)
    (read-only-mode nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("^/nix/store/.+\\.drv\\'" . nix-drv-mode))

(provide 'nix-drv-mode)
;;; nix-drv-mode.el ends here
