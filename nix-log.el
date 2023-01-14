;;; nix-log.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'files)

(defun nix-log-path (drv-file)
  "Get the nix log of path a derivation"
  (let* ((drv-name (file-relative-name drv-file nix-store-dir))
	 (log-file (format "%s/log/nix/drvs/%s/%s.bz2"
                           nix-state-dir
                           (substring drv-name 0 2) (substring drv-name 2))))
    (if (file-exists-p log-file) log-file
      (error "No log is available for derivation"))))

;;;###autoload
(defun nix-log (file attr)
  "Open the nix log.
FILE nix file to parse.
ATTR attribute to load the log of."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (let* ((drv-file (nix-instantiate file attr))
         (log-file (nix-log-path drv-file)))
    (find-file log-file)))

(provide 'nix-log)
;;; nix-log.el ends here
