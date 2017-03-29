;;; guix-location.el --- Package locations

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

;; This file provides the code to work with locations of Guix packages.

;;; Code:

(require 'cl-lib)
(require 'guix-repl)
(require 'guix-read)
(require 'guix-guile)

(defun guix-package-location (id-or-name)
  "Return location of a package with ID-OR-NAME.
For the meaning of location, see `guix-find-location'."
  (guix-eval-read (guix-make-guile-expression
                   'package-location-string id-or-name)))

;;;###autoload
(defun guix-find-location (location &optional directory)
  "Go to LOCATION of a package.
LOCATION is a string of the form:

  \"FILE:LINE:COLUMN\"

If FILE is relative, it is considered to be relative to
DIRECTORY (`guix-directory' by default).

Interactively, prompt for LOCATION.  With prefix argument, prompt
for DIRECTORY as well."
  (interactive
   (list (guix-read-package-location)
         (guix-read-directory)))
  (cl-multiple-value-bind (file line column)
      (split-string location ":")
    (find-file (expand-file-name file (or directory guix-directory)))
    (when (and line column)
      (let ((line   (string-to-number line))
            (column (string-to-number column)))
        (goto-char (point-min))
        (forward-line (- line 1))
        (move-to-column column)
        (recenter 1)))))

;;;###autoload
(defun guix-edit (id-or-name &optional directory)
  "Edit (go to location of) package with ID-OR-NAME.
See `guix-find-location' for the meaning of package location and
DIRECTORY.
Interactively, with prefix argument, prompt for DIRECTORY."
  (interactive
   (list (guix-read-package-name)
         (guix-read-directory)))
  (let ((loc (guix-package-location id-or-name)))
    (if loc
        (guix-find-location loc directory)
      (message "Couldn't find package location."))))

(provide 'guix-location)

;;; guix-location.el ends here
