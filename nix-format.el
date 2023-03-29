;;; nix-format.el --- Nix formatter -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; Homepage: https://github.com/NixOS/nix-mode

;;; Commentary:

;;; Code:

(defcustom nix-nixfmt-bin "nixfmt"
  "Path to nixfmt executable."
  :group 'nix
  :type 'string)

(defun nix--replace-buffer-contents (src dst)
  (if (fboundp 'replace-buffer-contents)
      (with-current-buffer dst (replace-buffer-contents src))
    (unless  (string= (with-current-buffer src (buffer-string))
		      (with-current-buffer dst (buffer-string)))
      (with-current-buffer src
	(copy-to-buffer dst (point-min) (point-max))))))

(defun nix--format-call (buf nixfmt-bin)
  "Format BUF using nixfmt."
  (with-current-buffer (get-buffer-create "*nixfmt*")
    (erase-buffer)
    (insert-buffer-substring buf)
    (if (zerop (call-process-region (point-min) (point-max) nixfmt-bin t t nil))
	(nix--replace-buffer-contents (current-buffer) buf)
      (error "Nixfmt failed, see *nixfmt* buffer for details"))))

(defun nix--find-nixfmt ()
  "Find the nixfmt binary, or error if it's missing."
  (let ((nixfmt-bin (executable-find nix-nixfmt-bin)))
    (unless nixfmt-bin
      (error "Could not locate executable %S" nix-nixfmt-bin))
    nixfmt-bin))

(defun nix-format-buffer ()
  "Format the current buffer using nixfmt."
  (interactive)
  (nix--format-call (current-buffer) (nix--find-nixfmt))
  (message "Formatted buffer with nixfmt."))

;;;###autoload
(defun nix-format-before-save ()
  "Add this to `before-save-hook' to run nixfmt when saving."
  (when (derived-mode-p 'nix-mode)
    (nix-format-buffer)))

(provide 'nix-format)
;;; nix-format.el ends here
