;;; hydra.el --- Common code for interacting with Hydra  -*- lexical-binding: t -*-

;; Copyright © 2015–2017 Alex Kost <alezost@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some general code for 'list'/'info' interfaces for
;; Hydra (Guix build farm).

;;; Code:

(require 'json)
(require 'bui)
(require 'nix)
(require 'nix-utils)

(nix-define-groups hydra)

(defvar nix-hydra-job-regexp
  (concat ".*\\." (regexp-opt nix-system-types) "\\'")
  "Regexp matching a full name of Hydra job (including system).")

(defun nix-hydra-job-name-specification (name version)
  "Return Hydra's job name specification by NAME and VERSION."
  (concat name "-" version))

(defun nix-hydra-message (entries search-type &rest _)
  "Display a message after showing Hydra ENTRIES."
  ;; XXX Add more messages maybe.
  (when (null entries)
    (if (eq search-type 'fake)
        (message "The update is impossible due to lack of Hydra API.")
      (message "Hydra has returned no results."))))

(defun nix-hydra-list-describe (&rest ids)
  "Describe 'hydra' entries with IDS (list of identifiers)."
  (bui-display-entries
   (bui-entries-by-ids (bui-current-entries) ids)
   (bui-current-entry-type) 'info
   ;; Hydra does not provide an API to receive builds/jobsets by
   ;; IDs/names, so we use a 'fake' search type.
   '(fake)
   'add))

;;; Readers

(defvar nix-hydra-projects
  '("nixpkgs" "nix" "disnix" "gnu" "hydra" "libchop" "nixops" "nixos"
    "node2nix" "patchelf")
  "List of available Hydra projects.")

(nix-define-readers
 :completions-var nix-hydra-projects
 :single-reader nix-hydra-read-project
 :single-prompt "Project: ")

(nix-define-readers
 :require-match nil
 :single-reader nix-hydra-read-jobset
 :single-prompt "Jobset: ")

(nix-define-readers
 :require-match nil
 :single-reader nix-hydra-read-job
 :single-prompt "Job: ")

(nix-define-readers
 :completions-var nix-help-system-types
 :single-reader nix-hydra-read-system
 :single-prompt "System: ")

;;; Defining URLs

(defvar nix-hydra-url "https://hydra.nixos.org"
  "URL of the Hydra build farm.")

(defun nix-hydra-url (&rest url-parts)
  "Return Hydra URL."
  (apply #'concat nix-hydra-url "/" url-parts))

(defun nix-hydra-api-url (type args)
  "Return URL for receiving data using Hydra API.
TYPE is the name of an allowed method.
ARGS is alist of (KEY . VALUE) pairs.
Skip ARG, if VALUE is nil or an empty string."
  (declare (indent 1))
  (let* ((fields (mapcar
                  (lambda (arg)
                    (pcase arg
                      (`(,key . ,value)
                       (unless (or (null value)
                                   (equal "" value))
                         (concat (nix-hexify key) "="
                                 (nix-hexify value))))
                      (_ (error "Wrong argument '%s'" arg))))
                  args))
         (fields (mapconcat #'identity (delq nil fields) "&")))
    (nix-hydra-url "api/" type "?" fields)))

;;; Receiving data from Hydra

(defun nix-hydra-receive-data (url)
  "Return output received from URL and processed with `json-read'."
  (with-temp-buffer
    (url-insert-file-contents url)
    (goto-char (point-min))
    (let ((json-key-type 'symbol)
          (json-array-type 'list)
          (json-object-type 'alist))
      (json-read))))

(defun nix-hydra-get-entries (entry-type search-type &rest args)
  "Receive ENTRY-TYPE entries from Hydra.
SEARCH-TYPE is one of the types defined by `nix-hydra-define-interface'."
  (unless (eq search-type 'fake)
    (let* ((url         (apply #'nix-hydra-search-url
                               entry-type search-type args))
           (raw-entries (nix-hydra-receive-data url))
           (entries     (apply #'nix-modify-objects
                               raw-entries
                               (nix-hydra-filters entry-type))))
      entries)))

;;; Filters for processing raw entries

(defun nix-hydra-filter-names (entry name-alist)
  "Replace names of ENTRY parameters using NAME-ALIST.
Each element of NAME-ALIST is (OLD-NAME . NEW-NAME) pair."
  (mapcar (lambda (param)
            (pcase param
              (`(,name . ,val)
               (let ((new-name (bui-assq-value name-alist name)))
                 (if new-name
                     (cons new-name val)
                   param)))))
          entry))

(defun nix-hydra-filter-boolean (entry params)
  "Convert number PARAMS (0/1) of ENTRY to boolean values (nil/t)."
  (mapcar (lambda (param)
            (pcase param
              (`(,name . ,val)
               (if (memq name params)
                   (cons name (nix-number->bool val))
                 param))))
          entry))

;;; Wrappers for defined variables

(defun nix-hydra-symbol (&rest symbols)
  "Return `SYMBOLS-...' symbol."
  (apply #'nix-make-symbol 'hydra symbols))

(defun nix-hydra-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE."
  (symbol-value (nix-hydra-symbol entry-type symbol)))

(defun nix-hydra-search-url (entry-type search-type &rest args)
  "Return URL to receive ENTRY-TYPE entries from Hydra."
  (apply (bui-assq-value (nix-hydra-symbol-value
                          entry-type 'search-types)
                         search-type)
         args))

(defun nix-hydra-filters (entry-type)
  "Return a list of filters for ENTRY-TYPE."
  (nix-hydra-symbol-value entry-type 'filters))

;;; Interface definers

(defmacro nix-hydra-define-entry-type (entry-type &rest args)
  "Define general code for ENTRY-TYPE.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Required keywords:

  - `:search-types' - default value of the generated
    `nix-hydra-ENTRY-TYPE-search-types' variable.

Optional keywords:

  - `:filters' - default value of the generated
    `nix-hydra-ENTRY-TYPE-filters' variable.

  - `:filter-names' - if specified, a generated
    `nix-hydra-ENTRY-TYPE-filter-names' function for filtering
    these names will be added to `nix-hydra-ENTRY-TYPE-filters'
    variable.

  - `:filter-boolean-params' - if specified, a generated
    `nix-hydra-ENTRY-TYPE-filter-boolean' function for filtering
    these names will be added to `nix-hydra-ENTRY-TYPE-filters'
    variable.

The rest keyword arguments are passed to
`bui-define-entry-type' macro."
  (declare (indent 1))
  (let* ((entry-type-str     (symbol-name entry-type))
         (full-entry-type    (nix-hydra-symbol entry-type))
         (prefix             (concat "nix-hydra-" entry-type-str))
         (search-types-var   (intern (concat prefix "-search-types")))
         (filters-var        (intern (concat prefix "-filters")))
         (get-fun            (intern (concat prefix "-get-entries"))))
    (bui-plist-let args
        ((search-types-val   :search-types)
         (filters-val        :filters)
         (filter-names-val   :filter-names)
         (filter-bool-val    :filter-boolean-params))
      `(progn
         (defvar ,search-types-var ,search-types-val
           ,(format "\
Alist of search types and according URL functions.
Functions are used to define URL to receive '%s' entries."
                    entry-type-str))

         (defvar ,filters-var ,filters-val
           ,(format "\
List of filters for '%s' parameters.
Each filter is a function that should take an entry as a single
argument, and should also return an entry."
                    entry-type-str))

         ,(when filter-bool-val
            (let ((filter-bool-var (intern (concat prefix
                                                   "-filter-boolean-params")))
                  (filter-bool-fun (intern (concat prefix
                                                   "-filter-boolean"))))
              `(progn
                 (defvar ,filter-bool-var ,filter-bool-val
                   ,(format "\
List of '%s' parameters that should be transformed to boolean values."
                            entry-type-str))

                 (defun ,filter-bool-fun (entry)
                   ,(format "\
Run `nix-hydra-filter-boolean' with `%S' variable."
                            filter-bool-var)
                   (nix-hydra-filter-boolean entry ,filter-bool-var))

                 (setq ,filters-var
                       (cons ',filter-bool-fun ,filters-var)))))

         ;; Do not move this clause up!: name filtering should be
         ;; performed before any other filtering, so this filter should
         ;; be consed after the boolean filter.
         ,(when filter-names-val
            (let* ((filter-names-var (intern (concat prefix
                                                     "-filter-names")))
                   (filter-names-fun filter-names-var))
              `(progn
                 (defvar ,filter-names-var ,filter-names-val
                   ,(format "\
Alist of '%s' parameter names returned by Hydra API and names
used internally by the elisp code of this package."
                            entry-type-str))

                 (defun ,filter-names-fun (entry)
                   ,(format "\
Run `nix-hydra-filter-names' with `%S' variable."
                            filter-names-var)
                   (nix-hydra-filter-names entry ,filter-names-var))

                 (setq ,filters-var
                       (cons ',filter-names-fun ,filters-var)))))

         (defun ,get-fun (search-type &rest args)
           ,(format "\
Receive '%s' entries.
See `nix-hydra-get-entries' for details."
                    entry-type-str)
           (apply #'nix-hydra-get-entries
                  ',entry-type search-type args))

         (bui-define-groups ,full-entry-type
           :parent-group nix-hydra
           :parent-faces-group nix-hydra-faces)

         (bui-define-entry-type ,full-entry-type
           :message-function 'nix-hydra-message
           ,@%foreign-args)))))

(defmacro nix-hydra-define-interface (entry-type buffer-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE hydra entries.

This macro should be called after calling
`nix-hydra-define-entry-type' with the same ENTRY-TYPE.

ARGS are passed to `bui-define-interface' macro."
  (declare (indent 2))
  `(bui-define-interface ,(nix-hydra-symbol entry-type) ,buffer-type
     :get-entries-function ',(nix-hydra-symbol entry-type 'get-entries)
     ,@args))

(defvar nix-hydra-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "nix-hydra-define-entry-type"
                           "nix-hydra-define-interface"))
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode nix-hydra-font-lock-keywords)

(provide 'nix-hydra)

;;; nix-hydra.el ends here
