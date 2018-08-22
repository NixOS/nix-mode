;;; nix-drv-mode.el --- Major mode for viewing .drv files

;; Maintainer: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Version: 1.2.1
;; Keywords: nix, languages, tools, unix
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A major mode for viewing Nix derivations (.drv files). See the Nix
;; manual for more information available at
;; https://nixos.org/nix/manual/.

;;; Code:

(require 'json-mode)
(require 'nix)

(defvar-local nix-drv-mode nil)

;;;###autoload
(defun nix-drv-mode ()
  "Pretty print Nix’s .drv files."
  (interactive)
  (when (string-match (format "^%s/" nix-store-dir) (buffer-file-name))
    (if nix-drv-mode
        (progn
          (erase-buffer)
          (insert-file-contents (buffer-file-name))
          (setq nix-drv-mode nil)
          (set-buffer-modified-p nil)
          (read-only-mode nil))
      (let ((inhibit-read-only t))
        (setq nix-drv-mode t)
        (erase-buffer)
        (insert (shell-command-to-string
                 (format "%s show-derivation \"%s\""
		         nix-executable
		         (buffer-file-name))))
        (json-mode)
        (set-buffer-modified-p nil)
        (read-only-mode 1)))))

(provide 'nix-drv-mode)
;;; nix-drv-mode.el ends here
