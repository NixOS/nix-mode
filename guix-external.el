;;; guix-external.el --- External programs  -*- lexical-binding: t -*-

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

;; This file provides auxiliary code for running external programs.

;;; Code:

(require 'cl-lib)
(require 'guix nil t)
(require 'guix-config)
(require 'guix-utils)

(defgroup guix-external nil
  "Settings for external programs."
  :group 'guix)

(defcustom guix-guile-program
  (or guix-config-guile-program
      (executable-find "guile"))
  "Name of the 'guile' executable used for Guix REPL.
May be either a string (the name of the executable) or a list of
strings of the form:

  (NAME . ARGS)

Where ARGS is a list of arguments to the guile program."
  :type 'string
  :group 'guix-external)

(defcustom guix-dot-program
  (executable-find "dot")
  "Name of the 'dot' executable."
  :type 'string
  :group 'guix-external)

(defcustom guix-dot-default-arguments
  '("-Tpng")
  "Default arguments for 'dot' program."
  :type '(repeat string)
  :group 'guix-external)

(defcustom guix-dot-file-name-function #'guix-png-file-name
  "Function used to define a file name of a temporary 'dot' file.
The function is called without arguments."
  :type '(choice (function-item guix-png-file-name)
                 (function :tag "Other function"))
  :group 'guix-external)

(defun guix-dot-arguments (output-file &rest args)
  "Return a list of dot arguments for writing a graph into OUTPUT-FILE.
If ARGS is nil, use `guix-dot-default-arguments'."
  (or guix-dot-program
      (error (concat "Couldn't find 'dot'.\n"
                     "Set `guix-dot-program' to a proper value")))
  (cl-list* guix-dot-program
            (concat "-o" output-file)
            (or args guix-dot-default-arguments)))

(defun guix-dot-file-name ()
  "Call `guix-dot-file-name-function'."
  (funcall guix-dot-file-name-function))

(defun guix-png-file-name ()
  "Return '.png' file name in the `guix-temporary-directory'."
  (guix-temporary-file-name "graph-" ".png"))

(defun guix-html-file-name ()
  "Return '.html' file name in the `guix-temporary-directory'."
  (guix-temporary-file-name "graph-" ".html"))

(provide 'guix-external)

;;; guix-external.el ends here
