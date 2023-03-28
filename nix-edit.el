;;; nix-edit.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-search)

;;;###autoload
(defun nix-edit (&optional file attr)
  "Open the nix log.
FILE the nix file to load from.
ATTR the attribute to find in nix expressions."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (let ((stdout (generate-new-buffer "nix-edit"))
        (process-environment (cons "EDITOR=echo" process-environment))
	result)
    (call-process nix-executable nil (list stdout nil) nil
		  "edit" "-f" file attr)
    (with-current-buffer stdout
      (when (zerop (buffer-size))
	(error
	 "Error: nix edit failed to produce any output"))
      (setq result (substring (buffer-string) 0 (- (buffer-size) 1))))
    (kill-buffer stdout)
    (find-file result)))

(provide 'nix-edit)
;;; nix-edit.el ends here
