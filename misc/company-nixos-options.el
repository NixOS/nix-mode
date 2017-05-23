;;; company-nixos-options.el --- Company Backend for nixos-options

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0") (nixos-options "0.0.1") (cl-lib "0.5.0"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for exploring the NixOS options.  Inspired by
;; https://nixos.org/nixos/options.html.

;;; Code:
(require 'nixos-options)
(require 'company)
(require 'cl-lib)

(defun company-nixos-options--doc-buffer (candidate)
  "Return documentation buffer for chosen CANDIDATE."
  (let ((doc (nixos-options-get-documentation-for-option
              (nixos-options-get-option-by-name candidate))))
    (and doc (nixos-options-doc-buffer doc))))

(defun company-nixos-options--candidates (prefix)
  (let ((res))
    (dolist (option nixos-options)
      (let ((name (nixos-options-get-name option)))
        (when (string-prefix-p prefix name)
          (push name res))))
    res))

(defun company-nixos-options--annotation (candidate)
  (let ((type (nixos-options-get-type
               (nixos-options-get-option-by-name
                candidate))))
    (format "  <%s>" type)))

(defun company-nixos--grab-symbol ()
  (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.")
                                            (point))))

(defun company-nixos--in-nix-context-p ()
  (or (eq major-mode 'nix-mode)
      (equal "nix" (file-name-extension
                    (buffer-file-name (current-buffer))))))

(defun company-nixos-options--prefix ()
  "Grab prefix at point."
  (and (company-nixos--in-nix-context-p)
       (or (company-nixos--grab-symbol)
           'stop)))

;;;###autoload
(defun company-nixos-options (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nixos-options))
    (prefix (company-nixos-options--prefix))
    (candidates (company-nixos-options--candidates arg))
    (doc-buffer (company-nixos-options--doc-buffer arg))
    (annotation (company-nixos-options--annotation arg))))

(provide 'company-nixos-options)
;;; company-nixos-options.el ends here

