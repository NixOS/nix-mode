;;; helm-nixos-options.el --- Helm Interface for nixos-options

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: 0.1.0
;; Package-Requires: ((nixos-options "0.0.1") (helm "1.5.6"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for exploring the NixOS options.  Inspired by
;; https://nixos.org/nixos/options.html.

;;; Code:
(require 'nixos-options)
(require 'helm)

(defun helm-source-nixos-options-search ()
  `((name . "NixOS Options")
    (requires-pattern . 2)
    (candidates . nixos-options)
    (follow . 1)
    (persistent-action . (lambda (f) (message (format "%s" (nixos-options-get-description f)))))
    (action . (("View documentation" . (lambda (f)
                                         (switch-to-buffer-other-window
                                          (nixos-options-doc-buffer
                                           (nixos-options-get-documentation-for-option f)))))
               ("Insert into buffer" . (lambda (f) (insert (nixos-options-get-name f))))
               ("Pretty print" . (lambda (f) (message "Pretty Printed: %s" (pp f))))
               ("Display name" . (lambda (f) (message "Name: %s" (nixos-options-get-name f))))))))

;;;###autoload
(defun helm-nixos-options ()
  (interactive)
  (helm :sources `(,(helm-source-nixos-options-search))
        :buffer "*helm-nixos-options*"))

(provide 'helm-nixos-options)
;;; helm-nixos-options.el ends here

