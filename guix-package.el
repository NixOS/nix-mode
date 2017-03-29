;;; guix-package.el --- Guix packages  -*- lexical-binding: t -*-

;; Copyright © 2014–2017 Alex Kost <alezost@gmail.com>

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

;; This file provides a general code related to Guix package.

;;; Code:

(require 'cl-lib)
(require 'guix-read)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-misc)

(defun guix-package-name-specification (name version &optional output)
  "Return Guix package specification by its NAME, VERSION and OUTPUT."
  (concat name "@" version
          (when output (concat ":" output))))

(defun guix-package-id-and-output-by-output-id (output-id)
  "Return a list (PACKAGE-ID OUTPUT) by OUTPUT-ID."
  (cl-multiple-value-bind (package-id-str output)
      (split-string output-id ":")
    (let ((package-id (string-to-number package-id-str)))
      (list (if (= 0 package-id) package-id-str package-id)
            output))))

(defun guix-package-build-log-file (id)
  "Return build log file name of a package defined by ID."
  (guix-eval-read
   (guix-make-guile-expression 'package-build-log-file id)))

(declare-function guix-build-log-find-file "guix-build-log" (file))

(defun guix-package-find-build-log (id)
  "Show build log of a package defined by ID."
  (require 'guix-build-log)
  (let ((file (guix-package-build-log-file id)))
    (if file
        (guix-build-log-find-file file)
      (message "Couldn't find the package build log."))))

(defun guix-package-source-file-name (package-id)
  "Return a store file name to a source of a package PACKAGE-ID."
  (message "Calculating the source derivation ...")
  (guix-eval-read
   (guix-make-guile-expression
    'package-source-file-name package-id)))

(defun guix-package-store-path (package-id)
  "Return a list of store directories of outputs of package PACKAGE-ID."
  (message "Calculating the package derivation ...")
  (guix-eval-read
   (guix-make-guile-expression
    'package-store-path package-id)))

(defvar guix-after-source-download-hook nil
  "Hook run after successful performing a 'source-download' operation.")

(defun guix-package-source-build-derivation (package-id &optional prompt)
  "Build source derivation of a package PACKAGE-ID.
Ask a user with PROMPT for continuing an operation."
  (when (or (not guix-operation-confirm)
            (guix-operation-prompt (or prompt
                                       "Build the source derivation?")))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'package-source-build-derivation
      package-id
      :use-substitutes? (or guix-use-substitutes 'f)
      :dry-run? (or guix-dry-run 'f))
     nil 'source-download)))

(defun guix-build-package (package-id &optional prompt)
  "Build package with PACKAGE-ID.
Ask a user with PROMPT for continuing the build operation."
  (when (or (not guix-operation-confirm)
            (guix-operation-prompt (or prompt "Build package?")))
    (guix-eval-in-repl
     (format (concat "(build-package* (package-by-id %d)"
                     " #:use-substitutes? %s"
                     " #:dry-run? %s)")
             package-id
             (guix-guile-boolean guix-use-substitutes)
             (guix-guile-boolean guix-dry-run)))))

(defun guix-read-package-size-type ()
  "Prompt a user for a package size type."
  (intern
   (completing-read "Size type (\"text\" or \"image\"): "
                    '("text" "image")
                    nil t nil nil "text")))

;;;###autoload
(defun guix-package-size (package-or-file &optional type)
  "Show size of PACKAGE-OR-FILE.
PACKAGE-OR-FILE should be either a package name or a store file name.
TYPE should be on of the following symbols: `text' (default) or `image'.
Interactively, prompt for a package name and size TYPE."
  (interactive
   (list (guix-read-package-name)
         (guix-read-package-size-type)))
  (cl-case (or type 'text)
    (text (guix-eval-in-repl
           (guix-make-guile-expression
            'guix-command "size" package-or-file)))
    (image (let ((map-file (guix-png-file-name)))
             (guix-command-output
              (list "size"
                    (concat "--map-file=" map-file)
                    package-or-file))
             (guix-find-file map-file)))
    (t (error "Unknown size type (should be `image' or `text'): %S"
              type))))

;;;###autoload
(defun guix-lint (package &optional checkers)
  "Lint PACKAGE using CHECKERS.
PACKAGE can be either a package name or a package ID.
CHECKERS is a list of checker names; if nil, use all checkers.

Interactively, prompt for PACKAGE name and use all checkers.
With prefix argument, also prompt for checkers (should be comma
separated).

See Info node `(guix) Invoking guix lint' for details about linting."
  (interactive
   (list (guix-read-package-name)
         (and current-prefix-arg
              (guix-read-lint-checker-names))))
  (guix-eval-in-repl
   (guix-make-guile-expression
    'lint-package package checkers)))

(provide 'guix-package)

;;; guix-package.el ends here
