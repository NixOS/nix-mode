;;; nixos-packages.el --- Interface for browsing and completing NixOS packages.

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for exploring the NixOS packages.  Inspired by
;; https://nixos.org/nixos/packages.html.

;;; Code:

(require 'json)

(defvar nixos-packages-json-file
  (expand-file-name "nixos-packages.json" spacemacs-cache-directory)
  "Where to store the nixos-packages in JSON format")

(defvar nixos-packages-name-indent-amount 0
  "Indent by the maximum length, plus a colon, plus two spaces.")

(setq nixos-packages
      (if (file-exists-p nixos-packages-json-file)
          (let ((json-key-type 'string))
            (json-read-file nixos-packages-json-file))
        (let* ((cmd "nix-env -qaP hello --json")
               (data (replace-regexp-in-string "\n\\'" ""
                                               (shell-command-to-string cmd)))
               (json-key-type 'string))
          (append-to-file data nil nixos-packages-json-file)
          (json-read-from-string data))))


(assoc "name" (car nixos-packages))
(assoc "system" (car nixos-packages))
(assoc "description" (assoc "meta" (car nixos-packages)))
(assoc "homepage" (assoc "meta" (car nixos-packages)))
(assoc "license" (assoc "meta" (car nixos-packages)))
(assoc "longDescription" (assoc "meta" (car nixos-packages)))
(assoc "platforms" (assoc "meta" (car nixos-packages)))
(assoc "position" (assoc "meta" (car nixos-packages)))
(assoc "maintainers" (assoc "meta" (car nixos-packages)))
(assoc "maintainers" (assoc "meta" (car nixos-packages)))

;; Macros for defining constants and functions for working with options
(defmacro define-nixos-packages-item (item long-name &optional isMeta?)
  (let* ((name-const (intern (concat "nixos-packages-" item)))
         (long-name-const (intern (concat "nixos-packages-" item "-long-name")))
         (long-name-length-plus-padding (+ 3 (length long-name)))
         (long-name-docstring (format "The long description for %s." item))
         (item-getter (intern (concat "nixos-packages-get-" item)))
         (item-getter-docstring
          (format "Get the value of %s from PACKAGE" item))
         (item-display (intern (concat "nixos-packages-display-" item)))
         (item-display-docstring
          (format "Display the value for %s from PACKAGE" item)))
    `(progn
       (defconst ,name-const ,item)
       (defconst ,long-name-const ,long-name ,long-name-docstring)
       (if (> ,long-name-length-plus-padding nixos-packages-name-indent-amount)
           (setq nixos-packages-name-indent-amount
                 ,long-name-length-plus-padding))
       (if ,isMeta?
           (defun ,item-getter (option)
             ,item-getter-docstring
             (cdr (assoc ,name-const (assoc "meta" option))))
         (defun ,item-getter (option)
           ,item-getter-docstring
           (cdr (assoc ,name-const option))))

       (defun ,item-display (option)
         ,item-display-docstring
         (let ((item (,item-getter option))
               (format-string
                (format "%%-%ds %%s\n" nixos-packages-name-indent-amount)))
           (if (not (null item))
               (format format-string (concat ,long-name-const ":") item)
             ""))))))

(define-nixos-packages-item "description" "Description" t)
(define-nixos-packages-item "homepage" "Home Page URL" t)
(define-nixos-packages-item "license" "License" t)
(define-nixos-packages-item "longDescription" "Long Description" t)
(define-nixos-packages-item "platforms" "Supported Platforms" t)
(define-nixos-packages-item "maintainers" "List of Maintainers" t)
(define-nixos-packages-item "position" "Path to the nix-expression with line" t)
(define-nixos-packages-item "name" "Name")
(define-nixos-packages-item "system" "System")

(defun nixos-packages--make-alist (package)
  (let ((name (car package))
        (data (cdr package))
        (default (nixos-options-get-default package))
        (example (nixos-options-get-example package)))
    (progn
      (if (not (null default))
          (setcdr (assoc nixos-options-default package)
                  (nixos-options--boolean-string default)))
      (if (not (null example))
          (setcdr (assoc nixos-options-example package)
                  (nixos-options--boolean-string example)))
      (add-to-list 'data `(,nixos-options-name . ,name))
      `(,name . ,data))))
