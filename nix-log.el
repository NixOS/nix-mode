;;; nix-log.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Package-Requires: ((emacs "24.1"))
;; Keywords: nix

;; Version: 1.4.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-search)
(require 'nix-instantiate)

;;;###autoload
(defun nix-log (file attr)
  "Open the nix log.
FILE nix file to parse.
ATTR attribute to load the log of."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (let* ((drv-file (nix-instantiate file attr))
         (drv-name (progn
                     (string-match (format "^%s/\\(.*\\)$" nix-store-dir) drv-file)
                     (match-string 1 drv-file)))
         (log-file (format "%s/log/nix/drvs/%s/%s.bz2"
                           nix-state-dir
                           (substring drv-name 0 2) drv-name)))
    (if (file-exists-p log-file)
        (find-file log-file)
      (error "No log is available for derivation"))))

(provide 'nix-log)
;;; nix-log.el ends here
