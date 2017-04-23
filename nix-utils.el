;;; nix-utils.el --- General utility functions  -*- lexical-binding: t -*-

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

(defun nix-concat-strings (strings separator &optional location)
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

(defun nix-hexify (value)
  "Convert VALUE to string and hexify it."
  (url-hexify-string (bui-get-string value)))

(defun nix-number->bool (number)
  "Convert NUMBER to boolean value.
Return nil, if NUMBER is 0; return t otherwise."
  (not (zerop number)))

(defun nix-list-maybe (object)
  "If OBJECT is list, return it; otherwise return (list OBJECT)."
  (if (listp object)
      object
    (list object)))

(defun nix-shell-quote-argument (argument)
  "Quote shell command ARGUMENT.
This function is similar to `shell-quote-argument', but less strict."
  (if (equal argument "")
      "''"
    (replace-regexp-in-string
     "\n" "'\n'"
     (replace-regexp-in-string
      (rx (not (any alnum "-=,./\n"))) "\\\\\\&" argument))))

(defun nix-command-symbol (&optional args)
  "Return symbol by concatenating 'nix' and ARGS (strings)."
  (intern (nix-concat-strings (cons "nix" args) "-")))

(defun nix-command-string (&optional args)
  "Return 'guix ARGS ...' string with quoted shell arguments."
  (let ((args (mapcar #'nix-shell-quote-argument args)))
    (nix-concat-strings (cons "nix" args) " ")))

(defun nix-copy-command-as-kill (args &optional no-message?)
  "Put 'guix ARGS ...' string into `kill-ring'.
See also `nix-copy-as-kill'."
  (bui-copy-as-kill (nix-command-string args) no-message?))

(defun nix-compose-buffer-name (base-name postfix)
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

(defun nix-completing-read (prompt table &optional predicate
                             require-match initial-input
                             hist def inherit-input-method)
  "Same as `completing-read' but return nil instead of an empty string."
  (let ((res (completing-read prompt table predicate
                              require-match initial-input
                              hist def inherit-input-method)))
    (unless (string= "" res) res)))

(defun nix-completing-read-multiple (prompt table &optional predicate
                                      require-match initial-input
                                      hist def inherit-input-method)
  "Same as `completing-read-multiple' but remove duplicates in result."
  (cl-remove-duplicates
   (completing-read-multiple prompt table predicate
                             require-match initial-input
                             hist def inherit-input-method)
   :test #'string=))

(declare-function org-read-date "org" t)

(defun nix-read-date (prompt)
  "Prompt for a date or time using `org-read-date'.
Return time value."
  (require 'org)
  (org-read-date nil t nil prompt))

(declare-function pcmpl-unix-user-names "pcmpl-unix")

(defun nix-read-user-name (&optional prompt initial-input)
  "Prompt for a user name using completions."
  (require 'pcmpl-unix)
  (nix-completing-read (or prompt "User name: ")
                        (pcmpl-unix-user-names)
                        nil nil initial-input))

(defun nix-read-file-name (prompt &optional dir default-filename
                                   mustmatch initial predicate)
  "Read file name.
This function is similar to `read-file-name' except it also
expands the file name."
  (expand-file-name (read-file-name prompt dir default-filename
                                    mustmatch initial predicate)))

(defcustom nix-find-file-function #'find-file
  "Function used to find a file.
This function is called by `nix-find-file' with a file name as a
single argument."
  :type '(choice (function-item find-file)
                 (function-item org-open-file)
                 (function :tag "Other function"))
  :group 'nix)

(defun nix-find-file (file)
  "Find FILE (using `nix-find-file-function') if it exists."
  (if (file-exists-p file)
      (funcall nix-find-file-function file)
    (message "File '%s' does not exist." file)))

(defvar url-handler-regexp)

(defun nix-find-file-or-url (file-or-url)
  "Find FILE-OR-URL."
  (require 'url-handlers)
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    (find-file file-or-url)))

(defun nix-switch-to-buffer-or-funcall (buffer-or-name function
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

(defun nix-pretty-print-buffer (buffer-or-name)
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

(defun nix-pretty-print-file (file-name &optional mode)
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
    (nix-pretty-print-buffer buffer)
    buffer))

(defmacro nix-while-search (regexp &rest body)
  "Evaluate BODY after each search for REGEXP in the current buffer."
  (declare (indent 1) (debug t))
  `(save-excursion
     (goto-char (point-min))
     (while (re-search-forward ,regexp nil t)
       ,@body)))

(defmacro nix-while-null (&rest body)
  "Evaluate BODY until its result becomes non-nil."
  (declare (indent 0) (debug t))
  (let ((result-var (make-symbol "result")))
    `(let (,result-var)
       (while (null ,result-var)
         (setq ,result-var ,@body))
       ,result-var)))

(defun nix-modify (object &rest modifiers)
  "Apply MODIFIERS to OBJECT.
OBJECT is passed as an argument to the first function from
MODIFIERS list, the returned result is passed to the second
function from the list and so on.  Return result of the last
modifier call."
  (if (null modifiers)
      object
    (apply #'nix-modify
           (funcall (car modifiers) object)
           (cdr modifiers))))

(defun nix-modify-objects (objects &rest modifiers)
  "Apply MODIFIERS to each object from a list of OBJECTS.
See `nix-modify' for details."
  (--map (apply #'nix-modify it modifiers)
         objects))

(defun nix-make-symbol (&rest symbols)
  "Return `nix-SYMBOLS-...' symbol."
  (apply #'bui-make-symbol 'nix symbols))

(defmacro nix-define-groups (name &rest args)
  "Define `nix-NAME' and `nix-NAME-faces' customization groups.
See `bui-define-groups' for details."
  (declare (indent 1))
  `(bui-define-groups ,(bui-make-symbol 'nix name)
     :parent-group nix
     :parent-faces-group nix-faces
     ,@args))

;;; Temporary file names

(defvar nix-temporary-directory nil
  "Directory for writing temporary Nix files.
If nil, it will be set when it will be used the first time.
This directory will be deleted on Emacs exit.")

(defun nix-temporary-directory ()
  "Return `nix-temporary-directory' (set it if needed)."
  (or (and nix-temporary-directory
           (file-exists-p nix-temporary-directory)
           nix-temporary-directory)
      (setq nix-temporary-directory
            (make-temp-file "emacs-nix-" 'dir))))

(defun nix-temporary-file-name (name &optional suffix)
  "Return file NAME from `nix-temporary-directory'.
If such file name already exists, or if SUFFIX string is
specified, make the returned name unique."
  (let* ((file-name (expand-file-name name (nix-temporary-directory)))
         (file-name (if suffix
                        (concat (make-temp-name file-name) suffix)
                      file-name)))
    (if (file-exists-p file-name)
        (nix-temporary-file-name name (or suffix ""))
      file-name)))

(defun nix-delete-temporary-directory ()
  "Delete `nix-temporary-directory' if it exists."
  (when (and nix-temporary-directory
             (file-exists-p nix-temporary-directory))
    (condition-case nil
        (delete-directory (nix-temporary-directory) 'recursive)
      (error
       (message "Failed to delete temporary Nix directory: %s"
                nix-temporary-directory)))))

(add-hook 'kill-emacs-hook 'nix-delete-temporary-directory)

;;; Fontification

(defvar nix-font-lock-flush-function
  (if (fboundp 'font-lock-flush)
      #'font-lock-flush         ; appeared in Emacs 25.1
    #'jit-lock-refontify)
  "Function used to refontify a buffer.

This function is called without arguments after
enabling/disabling `nix-prettify-mode',
`nix-build-log-minor-mode' and `nix-devel-mode'.

If nil, do not perform refontifying.")

(defun nix-font-lock-flush ()
  "Refontify the current buffer using `nix-font-lock-flush-function'."
  (when nix-font-lock-flush-function
    (if (fboundp nix-font-lock-flush-function)
        (funcall nix-font-lock-flush-function)
      (message "Unknown function: %S" nix-font-lock-flush-function))))

;;; Diff

(defvar nix-diff-switches "-u"
  "A string or list of strings specifying switches to be passed to diff.")

(defun nix-diff (old new &optional switches no-async)
  "Same as `diff', but use `nix-diff-switches' as default."
  (diff old new (or switches nix-diff-switches) no-async))

;;; Completing readers definers

(defmacro nix-define-reader (name read-fun completions prompt
                                  &optional require-match default)
  "Define NAME function to read from minibuffer.
READ-FUN may be `completing-read', `completing-read-multiple' or
another function with the same arguments."
  (declare (indent 1))
  `(defun ,name (&optional prompt initial-contents)
     (,read-fun (or prompt ,prompt)
                ,completions nil ,require-match
                initial-contents nil ,default)))

(defmacro nix-define-readers (&rest args)
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
            `(nix-define-reader ,single-reader
               nix-completing-read ,completions ,single-prompt
               ,require-match ,default))

         ,(when multiple-reader
            `(nix-define-reader ,multiple-reader
               completing-read-multiple ,completions ,multiple-prompt
               ,require-match ,default))

         ,(when (and multiple-reader multiple-separator)
            (let ((name (intern (concat (symbol-name multiple-reader)
                                        "-string"))))
              `(defun ,name (&optional prompt initial-contents)
                 (nix-concat-strings
                  (,multiple-reader prompt initial-contents)
                  ,multiple-separator))))))))

;;; Memoizing

(defun nix-memoize (function)
  "Return a memoized version of FUNCTION."
  (let ((cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (let ((result (gethash args cache 'not-found)))
        (if (eq result 'not-found)
            (let ((result (apply function args)))
              (puthash args result cache)
              result)
          result)))))

(defmacro nix-memoized-defun (name arglist docstring &rest body)
  "Define a memoized function NAME.
See `defun' for the meaning of arguments."
  (declare (doc-string 3) (indent 2))
  `(defalias ',name
     (nix-memoize (lambda ,arglist ,@body))
     ;; Add '(name args ...)' string with real arglist to the docstring,
     ;; because *Help* will display '(name &rest ARGS)' for a defined
     ;; function (since `nix-memoize' returns a lambda with '(&rest
     ;; args)').
     ,(format "(%S %s)\n\n%s"
              name
              (mapconcat #'symbol-name arglist " ")
              docstring)))

(defmacro nix-memoized-defalias (symbol definition &optional docstring)
  "Set SYMBOL's function definition to memoized version of DEFINITION."
  (declare (doc-string 3) (indent 1))
  `(defalias ',symbol
     (nix-memoize #',definition)
     ,(or docstring
          (format "Memoized version of `%S'." definition))))

(defvar nix-utils-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "nix-define-reader"
                           "nix-define-readers"
                           "nix-define-groups"
                           "nix-while-null"
                           "nix-while-search"))
            symbol-end)
       . 1)
      (,(rx "("
            (group "nix-memoized-" (or "defun" "defalias"))
            symbol-end
            (zero-or-more blank)
            (zero-or-one
             (group (one-or-more (or (syntax word) (syntax symbol))))))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode nix-utils-font-lock-keywords)

(provide 'nix-utils)

;;; nix-utils.el ends here
