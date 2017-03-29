;;; guix-geiser.el --- Interacting with Geiser  -*- lexical-binding: t -*-

;; Copyright © 2015 Alex Kost <alezost@gmail.com>

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

;; This file provides functions to evaluate guile code using Geiser.

;;; Code:

(require 'geiser-mode)
(require 'guix-guile)

(defun guix-geiser-repl ()
  "Return the current Geiser REPL."
  (or geiser-repl--repl
      (geiser-repl--repl/impl 'guile)
      (error "Geiser REPL not found")))

(defun guix-geiser-eval (str &optional repl)
  "Evaluate STR with guile expression using Geiser REPL.
If REPL is nil, use the current Geiser REPL.
Return a list of strings with result values of evaluation."
  (let ((gc-cons-threshold (max gc-cons-threshold 10000000)))
    (with-current-buffer (or repl (guix-geiser-repl))
      (let ((res (geiser-eval--send/wait `(:eval (:scm ,str)))))
        (if (geiser-eval--retort-error res)
            (error "Error in evaluating guile expression: %s"
                   (geiser-eval--retort-output res))
          (cdr (assq 'result res)))))))

(defun guix-geiser-eval-read (str &optional repl)
  "Evaluate STR with guile expression using Geiser REPL.
Return elisp expression of the first result value of evaluation."
  ;; The goal is to convert a string with scheme expression into elisp
  ;; expression.
  (let ((result (car (guix-geiser-eval str repl))))
    (cond
     ((or (string= result "#f")
          (string= result "#<unspecified>"))
      nil)
     ((string= result "#t")
      t)
     (t
      (read (replace-regexp-in-string
             "[ (]\\(#f\\)" "nil"
             (replace-regexp-in-string
              "[ (]\\(#t\\)" "t"
              result
              nil nil 1)
             nil nil 1))))))

(defun guix-geiser-eval-in-repl (str &optional repl no-history no-display)
  "Switch to Geiser REPL and evaluate STR with guile expression there.
If NO-HISTORY is non-nil, do not save STR in the REPL history.
If NO-DISPLAY is non-nil, do not switch to the REPL buffer."
  (let ((repl (or repl (guix-geiser-repl))))
    (with-current-buffer repl
      (geiser-repl--send str (not no-history)))
    (unless no-display
      (geiser-repl--switch-to-buffer repl))))

(defun guix-geiser-eval-in-repl-synchronously (str &optional repl
                                                   no-history no-display)
  "Evaluate STR in Geiser REPL synchronously, i.e. wait until the
REPL operation will be finished.
See `guix-geiser-eval-in-repl' for the meaning of arguments."
  (let* ((repl (if repl (get-buffer repl) (guix-geiser-repl)))
         (running? nil)
         (filter (lambda (output)
                   (setq running?
                         (and (get-buffer-process repl)
                              (not (guix-guile-prompt? output))))))
         (comint-output-filter-functions
          (cons filter comint-output-filter-functions)))
    (guix-geiser-eval-in-repl str repl no-history no-display)
    (while running?
      (sleep-for 0.1))))

(defun guix-geiser-call (proc &rest args)
  "Call (PROC ARGS ...) synchronously using the current Geiser REPL.
PROC and ARGS should be strings."
  (guix-geiser-eval
   (apply #'guix-guile-make-call-expression proc args)))

(defun guix-geiser-call-in-repl (proc &rest args)
  "Call (PROC ARGS ...) in the current Geiser REPL.
PROC and ARGS should be strings."
  (guix-geiser-eval-in-repl
   (apply #'guix-guile-make-call-expression proc args)))

(provide 'guix-geiser)

;;; guix-geiser.el ends here
