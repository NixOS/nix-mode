;;; guix-license.el --- Licenses

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

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

;; This file provides the code to work with licenses of Guix packages.

;;; Code:

(require 'guix-read)
(require 'guix-repl)
(require 'guix-guile)

(defun guix-license-file (&optional directory)
  "Return name of the file with license definitions.
DIRECTORY is a directory with Guix source (`guix-directory' by default)."
  (expand-file-name "guix/licenses.scm"
                    (or directory guix-directory)))

(defun guix-lookup-license-url (license)
  "Return URL of a LICENSE."
  (or (guix-eval-read (guix-make-guile-expression
                       'lookup-license-uri license))
      (error "Hm, I don't know URL of '%s' license" license)))

;;;###autoload
(defun guix-find-license-definition (license &optional directory)
  "Open licenses file from DIRECTORY and move to the LICENSE definition.
See `guix-license-file' for the meaning of DIRECTORY.
Interactively, with prefix argument, prompt for DIRECTORY."
  (interactive
   (list (guix-read-license-name)
         (guix-read-directory)))
  (find-file (guix-license-file directory))
  (goto-char (point-min))
  (when (re-search-forward (concat "\"" (regexp-quote license) "\"")
                           nil t)
    (beginning-of-defun)
    (recenter 1)))

;;;###autoload
(defun guix-browse-license-url (license)
  "Browse URL of a LICENSE."
  (interactive (list (guix-read-license-name)))
  (browse-url (guix-lookup-license-url license)))

(provide 'guix-license)

;;; guix-license.el ends here
