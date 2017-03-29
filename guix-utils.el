;;; guix-utils.el --- General utility functions  -*- lexical-binding: t -*-

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

;; This file provides auxiliary general code for Emacs-Guix package.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'bui-utils)
(require 'guix nil t)

(defun guix-guixsd? ()
  "Return non-nil, if current system is GuixSD."
  (file-exists-p "/run/current-system"))

(defun guix-concat-strings (strings separator &optional location)
  "Return new string by concatenating STRINGS with SEPARATOR.
If LOCATION is a symbol `head', add another SEPARATOR to the
beginning of the returned string; if `tail' - add SEPARATOR to
the end of the string; if nil, do not add SEPARATOR; otherwise
add both to the end and to the beginning."
  (let ((str (mapconcat #'identity strings separator)))
    (cond ((null location)
           str)
          ((eq location 'head)
           (concat separator str))
          ((eq location 'tail)
           (concat str separator))
          (t
           (concat separator str separator)))))

(defun guix-hexify (value)
  "Convert VALUE to string and hexify it."
  (url-hexify-string (bui-get-string value)))

(defun guix-number->bool (number)
  "Convert NUMBER to boolean value.
Return nil, if NUMBER is 0; return t otherwise."
  (not (zerop number)))

(defun guix-list-maybe (object)
  "If OBJECT is list, return it; otherwise return (list OBJECT)."
  (if (listp object)
      object
    (list object)))

(defun guix-shell-quote-argument (argument)
  "Quote shell command ARGUMENT.
This function is similar to `shell-quote-argument', but less strict."
  (if (equal argument "")
      "''"
    (replace-regexp-in-string
     "\n" "'\n'"
     (replace-regexp-in-string
      (rx (not (any alnum "-=,./\n"))) "\\\\\\&" argument))))

(defun guix-command-symbol (&optional args)
  "Return symbol by concatenating 'guix' and ARGS (strings)."
  (intern (guix-concat-strings (cons "guix" args) "-")))

(defun guix-command-string (&optional args)
  "Return 'guix ARGS ...' string with quoted shell arguments."
  (let ((args (mapcar #'guix-shell-quote-argument args)))
    (guix-concat-strings (cons "guix" args) " ")))

(defun guix-copy-command-as-kill (args &optional no-message?)
  "Put 'guix ARGS ...' string into `kill-ring'.
See also `guix-copy-as-kill'."
  (bui-copy-as-kill (guix-command-string args) no-message?))

(defun guix-compose-buffer-name (base-name postfix)
  "Return buffer name by appending BASE-NAME and POSTFIX.

In a simple case the result is:

  BASE-NAME: POSTFIX

If BASE-NAME is wrapped by '*', then the result is:

  *BASE-NAME: POSTFIX*"
  (let ((re (rx string-start
                (group (? "*"))
                (group (*? any))
                (group (? "*"))
                string-end)))
    (or (string-match re base-name)
        (error "Unexpected error in defining buffer name"))
    (let ((first*    (match-string 1 base-name))
          (name-body (match-string 2 base-name))
          (last*     (match-string 3 base-name)))
      ;; Handle the case when buffer name is wrapped by '*'.
      (if (and (string= "*" first*)
               (string= "*" last*))
          (concat "*" name-body ": " postfix "*")
        (concat base-name ": " postfix)))))

(defun guix-completing-read (prompt table &optional predicate
                             require-match initial-input
                             hist def inherit-input-method)
  "Same as `completing-read' but return nil instead of an empty string."
  (let ((res (completing-read prompt table predicate
                              require-match initial-input
                              hist def inherit-input-method)))
    (unless (string= "" res) res)))

(defun guix-completing-read-multiple (prompt table &optional predicate
                                      require-match initial-input
                                      hist def inherit-input-method)
  "Same as `completing-read-multiple' but remove duplicates in result."
  (cl-remove-duplicates
   (completing-read-multiple prompt table predicate
                             require-match initial-input
                             hist def inherit-input-method)
   :test #'string=))

(declare-function org-read-date "org" t)

(defun guix-read-date (prompt)
  "Prompt for a date or time using `org-read-date'.
Return time value."
  (require 'org)
  (org-read-date nil t nil prompt))

(declare-function pcmpl-unix-user-names "pcmpl-unix")

(defun guix-read-user-name (&optional prompt initial-input)
  "Prompt for a user name using completions."
  (require 'pcmpl-unix)
  (guix-completing-read (or prompt "User name: ")
                        (pcmpl-unix-user-names)
                        nil nil initial-input))

(defun guix-read-file-name (prompt &optional dir default-filename
                                   mustmatch initial predicate)
  "Read file name.
This function is similar to `read-file-name' except it also
expands the file name."
  (expand-file-name (read-file-name prompt dir default-filename
                                    mustmatch initial predicate)))

(defcustom guix-find-file-function #'find-file
  "Function used to find a file.
This function is called by `guix-find-file' with a file name as a
single argument."
  :type '(choice (function-item find-file)
                 (function-item org-open-file)
                 (function :tag "Other function"))
  :group 'guix)

(defun guix-find-file (file)
  "Find FILE (using `guix-find-file-function') if it exists."
  (if (file-exists-p file)
      (funcall guix-find-file-function file)
    (message "File '%s' does not exist." file)))

(defvar url-handler-regexp)

(defun guix-find-file-or-url (file-or-url)
  "Find FILE-OR-URL."
  (require 'url-handlers)
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    (find-file file-or-url)))

(defun guix-switch-to-buffer-or-funcall (buffer-or-name function
                                         &optional message)
  "Switch to BUFFER-OR-NAME if it exists.
If BUFFER-OR-NAME does not exist, call FUNCTION without
arguments, also display a message if MESSAGE is specified (it can
be either nil, a string, or another value for a default
message)."
  (let ((buffer (get-buffer buffer-or-name)))
    (if buffer
        (progn
          (switch-to-buffer buffer)
          (when message
            (message (if (stringp message)
                         message
                       (substitute-command-keys "\
Press '\\[revert-buffer]' to update this buffer.")))))
      (funcall function))))

(defun guix-pretty-print-buffer (buffer-or-name)
  "Pretty-print the contents of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    (goto-char (point-max))
    (let (sexp-beg)
      (while (setq sexp-beg (scan-sexps (point) -1))
        (goto-char sexp-beg)
        (delete-horizontal-space t)
        (unless (= (point) (line-beginning-position))
          (insert "\n"))
        (indent-pp-sexp 'pp)))))

(defun guix-pretty-print-file (file-name &optional mode)
  "Show FILE-NAME contents in MODE and pretty-print it.
If MODE is nil, use `scheme-mode'.
Put the point in the beginning of buffer.
Return buffer with the prettified contents."
  (let* ((base-name (file-name-nondirectory file-name))
         (buffer    (generate-new-buffer base-name)))
    (with-current-buffer buffer
      (insert-file-contents file-name)
      (goto-char (point-min))
      (funcall (or mode 'scheme-mode)))
    (guix-pretty-print-buffer buffer)
    buffer))

(defmacro guix-while-search (regexp &rest body)
  "Evaluate BODY after each search for REGEXP in the current buffer."
  (declare (indent 1) (debug t))
  `(save-excursion
     (goto-char (point-min))
     (while (re-search-forward ,regexp nil t)
       ,@body)))

(defmacro guix-while-null (&rest body)
  "Evaluate BODY until its result becomes non-nil."
  (declare (indent 0) (debug t))
  (let ((result-var (make-symbol "result")))
    `(let (,result-var)
       (while (null ,result-var)
         (setq ,result-var ,@body))
       ,result-var)))

(defun guix-modify (object &rest modifiers)
  "Apply MODIFIERS to OBJECT.
OBJECT is passed as an argument to the first function from
MODIFIERS list, the returned result is passed to the second
function from the list and so on.  Return result of the last
modifier call."
  (if (null modifiers)
      object
    (apply #'guix-modify
           (funcall (car modifiers) object)
           (cdr modifiers))))

(defun guix-modify-objects (objects &rest modifiers)
  "Apply MODIFIERS to each object from a list of OBJECTS.
See `guix-modify' for details."
  (--map (apply #'guix-modify it modifiers)
         objects))

(defun guix-make-symbol (&rest symbols)
  "Return `guix-SYMBOLS-...' symbol."
  (apply #'bui-make-symbol 'guix symbols))

(defmacro guix-define-groups (name &rest args)
  "Define `guix-NAME' and `guix-NAME-faces' customization groups.
See `bui-define-groups' for details."
  (declare (indent 1))
  `(bui-define-groups ,(bui-make-symbol 'guix name)
     :parent-group guix
     :parent-faces-group guix-faces
     ,@args))


;;; Temporary file names

(defvar guix-temporary-directory nil
  "Directory for writing temporary Guix files.
If nil, it will be set when it will be used the first time.
This directory will be deleted on Emacs exit.")

(defun guix-temporary-directory ()
  "Return `guix-temporary-directory' (set it if needed)."
  (or (and guix-temporary-directory
           (file-exists-p guix-temporary-directory)
           guix-temporary-directory)
      (setq guix-temporary-directory
            (make-temp-file "emacs-guix-" 'dir))))

(defun guix-temporary-file-name (name &optional suffix)
  "Return file NAME from `guix-temporary-directory'.
If such file name already exists, or if SUFFIX string is
specified, make the returned name unique."
  (let* ((file-name (expand-file-name name (guix-temporary-directory)))
         (file-name (if suffix
                        (concat (make-temp-name file-name) suffix)
                      file-name)))
    (if (file-exists-p file-name)
        (guix-temporary-file-name name (or suffix ""))
      file-name)))

(defun guix-delete-temporary-directory ()
  "Delete `guix-temporary-directory' if it exists."
  (when (and guix-temporary-directory
	     (file-exists-p guix-temporary-directory))
    (condition-case nil
	(delete-directory (guix-temporary-directory) 'recursive)
      (error
       (message "Failed to delete temporary Guix directory: %s"
		guix-temporary-directory)))))

(add-hook 'kill-emacs-hook 'guix-delete-temporary-directory)


;;; Fontification

(defvar guix-font-lock-flush-function
  (if (fboundp 'font-lock-flush)
      #'font-lock-flush         ; appeared in Emacs 25.1
    #'jit-lock-refontify)
  "Function used to refontify a buffer.

This function is called without arguments after
enabling/disabling `guix-prettify-mode',
`guix-build-log-minor-mode' and `guix-devel-mode'.

If nil, do not perform refontifying.")

(defun guix-font-lock-flush ()
  "Refontify the current buffer using `guix-font-lock-flush-function'."
  (when guix-font-lock-flush-function
    (if (fboundp guix-font-lock-flush-function)
        (funcall guix-font-lock-flush-function)
      (message "Unknown function: %S" guix-font-lock-flush-function))))


;;; Diff

(defvar guix-diff-switches "-u"
  "A string or list of strings specifying switches to be passed to diff.")

(defun guix-diff (old new &optional switches no-async)
  "Same as `diff', but use `guix-diff-switches' as default."
  (diff old new (or switches guix-diff-switches) no-async))


;;; Completing readers definers

(defmacro guix-define-reader (name read-fun completions prompt
                                   &optional require-match default)
  "Define NAME function to read from minibuffer.
READ-FUN may be `completing-read', `completing-read-multiple' or
another function with the same arguments."
  (declare (indent 1))
  `(defun ,name (&optional prompt initial-contents)
     (,read-fun (or prompt ,prompt)
                ,completions nil ,require-match
                initial-contents nil ,default)))

(defmacro guix-define-readers (&rest args)
  "Define reader functions.

ARGS should have a form [KEYWORD VALUE] ...  The following
keywords are available:

  - `completions-var' - variable used to get completions.

  - `completions-getter' - function used to get completions.

  - `require-match' - if the match is required (see
    `completing-read' for details); default is t.

  - `default' - default value.

  - `single-reader', `single-prompt' - name of a function to read
    a single value, and a prompt for it.

  - `multiple-reader', `multiple-prompt' - name of a function to
    read multiple values, and a prompt for it.

  - `multiple-separator' - if specified, another
    `<multiple-reader-name>-string' function returning a string
    of multiple values separated the specified separator will be
    defined."
  (bui-plist-let args
      ((completions-var    :completions-var)
       (completions-getter :completions-getter)
       (require-match      :require-match t)
       (default            :default)
       (single-reader      :single-reader)
       (single-prompt      :single-prompt)
       (multiple-reader    :multiple-reader)
       (multiple-prompt    :multiple-prompt)
       (multiple-separator :multiple-separator))
    (let ((completions
           (cond ((and completions-var completions-getter)
                  `(or ,completions-var
                       (setq ,completions-var
                             (funcall ',completions-getter))))
                 (completions-var
                  completions-var)
                 (completions-getter
                  `(funcall ',completions-getter)))))
      `(progn
         ,(when (and completions-var
                     (not (boundp completions-var)))
            `(defvar ,completions-var nil))

         ,(when single-reader
            `(guix-define-reader ,single-reader
               guix-completing-read ,completions ,single-prompt
               ,require-match ,default))

         ,(when multiple-reader
            `(guix-define-reader ,multiple-reader
               completing-read-multiple ,completions ,multiple-prompt
               ,require-match ,default))

         ,(when (and multiple-reader multiple-separator)
            (let ((name (intern (concat (symbol-name multiple-reader)
                                        "-string"))))
              `(defun ,name (&optional prompt initial-contents)
                 (guix-concat-strings
                  (,multiple-reader prompt initial-contents)
                  ,multiple-separator))))))))


;;; Memoizing

(defun guix-memoize (function)
  "Return a memoized version of FUNCTION."
  (let ((cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (let ((result (gethash args cache 'not-found)))
        (if (eq result 'not-found)
            (let ((result (apply function args)))
              (puthash args result cache)
              result)
          result)))))

(defmacro guix-memoized-defun (name arglist docstring &rest body)
  "Define a memoized function NAME.
See `defun' for the meaning of arguments."
  (declare (doc-string 3) (indent 2))
  `(defalias ',name
     (guix-memoize (lambda ,arglist ,@body))
     ;; Add '(name args ...)' string with real arglist to the docstring,
     ;; because *Help* will display '(name &rest ARGS)' for a defined
     ;; function (since `guix-memoize' returns a lambda with '(&rest
     ;; args)').
     ,(format "(%S %s)\n\n%s"
              name
              (mapconcat #'symbol-name arglist " ")
              docstring)))

(defmacro guix-memoized-defalias (symbol definition &optional docstring)
  "Set SYMBOL's function definition to memoized version of DEFINITION."
  (declare (doc-string 3) (indent 1))
  `(defalias ',symbol
     (guix-memoize #',definition)
     ,(or docstring
          (format "Memoized version of `%S'." definition))))


(defvar guix-utils-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "guix-define-reader"
                           "guix-define-readers"
                           "guix-define-groups"
                           "guix-while-null"
                           "guix-while-search"))
            symbol-end)
       . 1)
      (,(rx "("
            (group "guix-memoized-" (or "defun" "defalias"))
            symbol-end
            (zero-or-more blank)
            (zero-or-one
             (group (one-or-more (or (syntax word) (syntax symbol))))))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode guix-utils-font-lock-keywords)

(provide 'guix-utils)

;;; guix-utils.el ends here
