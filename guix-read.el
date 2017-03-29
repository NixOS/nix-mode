;;; guix-read.el --- Minibuffer readers

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

;; This file provides functions to prompt a user for packages, system
;; types, hash formats and other guix related stuff.

;;; Code:

(require 'guix-help-vars)
(require 'guix-utils)
(require 'guix-repl)


;;; Receivable lists of packages, lint checkers, etc.

(guix-memoized-defun guix-graph-backend-names ()
  "Return a list of names of available graph backends."
  (guix-eval-read "(graph-backend-names)"))

(guix-memoized-defun guix-graph-node-type-names ()
  "Return a list of names of available graph node types."
  (guix-eval-read "(graph-node-type-names)"))

(guix-memoized-defun guix-refresh-updater-names ()
  "Return a list of names of available refresh updater types."
  (guix-eval-read "(refresh-updater-names)"))

(guix-memoized-defun guix-lint-checker-names ()
  "Return a list of names of available lint checkers."
  (guix-eval-read "(lint-checker-names)"))

(guix-memoized-defun guix-compressor-names ()
  "Return a list of names of available compressors."
  (guix-eval-read "(compressor-names)"))

(guix-memoized-defun guix-pack-format-names ()
  "Return a list of names of available pack formats."
  (guix-eval-read "(pack-format-names)"))

(guix-memoized-defun guix-package-names ()
  "Return a list of names of available packages."
  (sort (guix-eval-read "(package-names)")
        #'string<))

(guix-memoized-defun guix-license-names ()
  "Return a list of names of available licenses."
  (guix-eval-read "(license-names)"))

(guix-memoized-defun guix-package-locations ()
  "Return a list of available package locations."
  (sort (guix-eval-read "(package-location-files)")
        #'string<))


;;; Readers

(guix-define-readers
 :completions-var guix-help-system-types
 :single-reader guix-read-system-type
 :single-prompt "System type: ")

(guix-define-readers
 :completions-var guix-help-source-types
 :single-reader guix-read-source-type
 :single-prompt "Source type: ")

(guix-define-readers
 :completions-var guix-help-hash-formats
 :default "nix-base32"
 :single-reader guix-read-hash-format
 :single-prompt "Hash format: ")

(guix-define-readers
 :completions-var guix-help-refresh-subsets
 :single-reader guix-read-refresh-subset
 :single-prompt "Refresh subset: ")

(guix-define-readers
 :completions-getter guix-refresh-updater-names
 :multiple-reader guix-read-refresh-updater-names
 :multiple-prompt "Refresh updater,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-var guix-help-key-policies
 :default "interactive"
 :single-reader guix-read-key-policy
 :single-prompt "Key policy: ")

(guix-define-readers
 :completions-var guix-help-elpa-archives
 :default "gnu"
 :single-reader guix-read-elpa-archive
 :single-prompt "ELPA archive: ")

(guix-define-readers
 :completions-var guix-help-verify-options
 :multiple-reader guix-read-verify-options
 :multiple-prompt "Verify option,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-getter guix-graph-backend-names
 :default "graphviz"
 :single-reader guix-read-graph-backend
 :single-prompt "Graph backend: ")

(guix-define-readers
 :completions-getter guix-graph-node-type-names
 :default "package"
 :single-reader guix-read-graph-node-type
 :single-prompt "Graph node type: ")

(guix-define-readers
 :completions-getter guix-lint-checker-names
 :multiple-reader guix-read-lint-checker-names
 :multiple-prompt "Linter,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-getter guix-compressor-names
 :single-reader guix-read-compressor-name
 :single-prompt "Compressor: ")

(guix-define-readers
 :completions-getter guix-pack-format-names
 :single-reader guix-read-pack-format-name
 :single-prompt "Pack format: ")

(guix-define-readers
 :completions-getter guix-package-names
 :require-match nil
 :single-reader guix-read-package-name
 :single-prompt "Package: "
 :multiple-reader guix-read-package-names
 :multiple-prompt "Package,s: "
 :multiple-separator " ")

(guix-define-readers
 :completions-getter guix-license-names
 :single-reader guix-read-license-name
 :single-prompt "License: ")

(guix-define-readers
 :completions-getter guix-package-locations
 :single-reader guix-read-package-location
 :single-prompt "Location: ")

(provide 'guix-read)

;;; guix-read.el ends here
