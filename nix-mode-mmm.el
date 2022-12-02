;;; nix-mode-mmm.el --- Support for MMM in nix-mode -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix
;; Version: 1.5.0
;; Package-Requires: ((emacs "24.3") (mmm-mode "0.5.8"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'mmm-mode)

(mmm-add-group 'nix-sh
               '((sh-command
                  :submode sh-mode
                  :face mmm-output-submode-face
                  :front "[^'a-zA-Z]''[^']"
                  :back "''[^$\\']"
                  :include-front t
                  :front-offset 4
                  :end-not-begin t
                  )))

;; (setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'nix-mode "\\.nix\\'" 'nix-sh)

(provide 'nix-mode-mmm)
;;; nix-mode-mmm.el ends here
