;;; guix-guile.el --- Auxiliary tools for working with Guile code  -*- lexical-binding: t -*-

;; Copyright © 2015, 2017 Alex Kost <alezost@gmail.com>

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

;; This file provides functions for parsing guile code, making guile
;; expressions, etc.

;;; Code:

(require 'geiser-guile)

(defvar guix-guile-definition-regexp
  (rx bol "(define"
      (zero-or-one "*")
      (zero-or-one "-public")
      (one-or-more space)
      (zero-or-one "(")
      (group (one-or-more (or word (syntax symbol)))))
  "Regexp used to find the guile definition.")

(defun guix-guile-current-definition ()
  "Return string with name of the current top-level guile definition."
  (save-excursion
    (beginning-of-defun)
    (if (looking-at guix-guile-definition-regexp)
        (match-string-no-properties 1)
      (error "Couldn't find the current definition"))))

(defun guix-guile-current-module ()
  "Return a string with the current guile module.
Return nil, if current buffer does not define a module."
  ;; Modified version of `geiser-guile--get-module'.
  (save-excursion
    (geiser-syntax--pop-to-top)
    (when (or (re-search-backward geiser-guile--module-re nil t)
              (looking-at geiser-guile--library-re)
              (re-search-forward geiser-guile--module-re nil t))
      (match-string-no-properties 1))))

(defun guix-guile-boolean (arg)
  "Return a string with guile boolean value.
Transform elisp ARG (nil or non-nil) to the guile boolean (#f or #t)."
  (if arg "#t" "#f"))

(defun guix-guile-keyword-regexp (keyword)
  "Return regexp to find guile KEYWORD."
  (format "(\\(%s\\)\\_>" keyword))

(defun guix-guile-make-call-expression (proc &rest args)
  "Return \"(PROC ARGS ...)\" string.
PROC and ARGS should be strings."
  (format "(%s %s)"
          proc
          (mapconcat #'identity args " ")))

(defun guix-make-guile-expression (fun &rest args)
  "Return string containing a guile expression for calling FUN with ARGS."
  (format "(%S %s)" fun
          (mapconcat
           (lambda (arg)
             (cond
              ((null arg) "'()")
              ((stringp arg)
               (prin1-to-string (substring-no-properties arg)))
              ((or (eq arg t)
                   ;; An ugly hack to separate 'false' from nil.
                   (equal arg 'f)
                   (keywordp arg))
               (concat "#" (prin1-to-string arg t)))
              ((or (symbolp arg) (listp arg))
               (concat "'" (prin1-to-string arg)))
              (t (prin1-to-string arg))))
           args
           " ")))

(defun guix-guile-prompt? (string)
  "Return non-nil, if STRING contains a Guile prompt."
  (or (string-match-p geiser-guile--prompt-regexp string)
      (string-match-p geiser-guile--debugger-prompt-regexp string)))

(provide 'guix-guile)

;;; guix-guile.el ends here
