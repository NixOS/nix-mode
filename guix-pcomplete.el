;;; guix-pcomplete.el --- Functions for completing guix commands  -*- lexical-binding: t -*-

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

;; This file provides completions for "guix" command that may be used in
;; `shell', `eshell' and wherever `pcomplete' works.

;;; Code:

(require 'pcomplete)
(require 'pcmpl-unix)
(require 'cl-lib)
(require 'guix nil t)
(require 'guix-read)
(require 'guix-misc)
(require 'guix-utils)
(require 'guix-help-vars)


;;; Parsing guix output

(defun guix-pcomplete-search-in-help (regexp &optional group
                                             &rest args)
  "Search for REGEXP in 'guix ARGS... --help' output.
Return a list of strings matching REGEXP.
GROUP specifies a parenthesized expression used in REGEXP."
  (with-temp-buffer
    (insert (guix-help-string args))
    (let (result)
      (guix-while-search regexp
        (push (match-string-no-properties group) result))
      (nreverse result))))

(defmacro guix-pcomplete-define-options-finder (name docstring regexp
                                                     &optional filter)
  "Define function NAME to receive guix options and commands.

The defined function takes rest COMMANDS argument.  This function
will search for REGEXP in 'guix COMMANDS... --help' output (or
'guix --help' if COMMANDS is nil) using
`guix-pcomplete-search-in-help' and will return its result.

If FILTER is specified, it should be a function.  The result is
passed to this FILTER as argument and the result value of this
function call is returned."
  (declare (doc-string 2) (indent 1))
  `(guix-memoized-defun ,name (&rest commands)
     ,docstring
     (let ((res (apply #'guix-pcomplete-search-in-help
                       ,regexp guix-help-parse-regexp-group commands)))
       ,(if filter
            `(funcall ,filter res)
          'res))))

(guix-pcomplete-define-options-finder guix-pcomplete-commands
  "If COMMANDS is nil, return a list of available guix commands.
If COMMANDS is non-nil (it should be a list of strings), return
available subcommands, actions, etc. for 'guix COMMANDS'."
  guix-help-parse-command-regexp)

(guix-pcomplete-define-options-finder guix-pcomplete-long-options
  "Return a list of available long options for 'guix COMMANDS'."
  guix-help-parse-long-option-regexp)

(guix-pcomplete-define-options-finder guix-pcomplete-short-options
  "Return a string with available short options for 'guix COMMANDS'."
  guix-help-parse-short-option-regexp
  (lambda (list)
    (guix-concat-strings list "")))


;;; Completing

(defvar guix-pcomplete-option-regexp (rx string-start "-")
  "Regexp to match an option.")

(defvar guix-pcomplete-long-option-regexp (rx string-start "--")
  "Regexp to match a long option.")

(defvar guix-pcomplete-long-option-with-arg-regexp
  (rx string-start
      (group "--" (one-or-more any)) "="
      (group (zero-or-more any)))
  "Regexp to match a long option with its argument.
The first parenthesized group defines the option and the second
group - the argument.")

(defvar guix-pcomplete-short-option-with-arg-regexp
  (rx string-start
      (group "-" (not (any "-")))
      (group (zero-or-more any)))
  "Regexp to match a short option with its argument.
The first parenthesized group defines the option and the second
group - the argument.")

(defun guix-pcomplete-match-option ()
  "Return non-nil, if the current argument is an option."
  (pcomplete-match guix-pcomplete-option-regexp 0))

(defun guix-pcomplete-match-long-option ()
  "Return non-nil, if the current argument is a long option."
  (pcomplete-match guix-pcomplete-long-option-regexp 0))

(defun guix-pcomplete-match-long-option-with-arg ()
  "Return non-nil, if the current argument is a long option with value."
  (pcomplete-match guix-pcomplete-long-option-with-arg-regexp 0))

(defun guix-pcomplete-match-short-option-with-arg ()
  "Return non-nil, if the current argument is a short option with value."
  (pcomplete-match guix-pcomplete-short-option-with-arg-regexp 0))

(defun guix-pcomplete-long-option-arg (option args)
  "Return a long OPTION's argument from a list of arguments ARGS."
  (let* ((re (concat "\\`" option "=\\(.*\\)"))
         (args (cl-member-if (lambda (arg)
                               (string-match re arg))
                             args))
         (cur (car args)))
    (when cur
      (match-string-no-properties 1 cur))))

(defun guix-pcomplete-short-option-arg (option args)
  "Return a short OPTION's argument from a list of arguments ARGS."
  (let* ((re (concat "\\`" option "\\(.*\\)"))
         (args (cl-member-if (lambda (arg)
                               (string-match re arg))
                             args))
         (cur (car args)))
    (when cur
      (let ((arg (match-string-no-properties 1 cur)))
        (if (string= "" arg)
            (cadr args)                 ; take the next arg
          arg)))))

(defun guix-pcomplete-complete-comma-args (entries)
  "Complete comma separated arguments using ENTRIES."
  (let ((index pcomplete-index))
    (while (= index pcomplete-index)
      (let* ((args (if (or (guix-pcomplete-match-long-option-with-arg)
                           (guix-pcomplete-match-short-option-with-arg))
                       (pcomplete-match-string 2 0)
                     (pcomplete-arg 0)))
             (input (if (string-match ".*,\\(.*\\)" args)
                        (match-string-no-properties 1 args)
                      args)))
        (pcomplete-here* entries input)))))

(defun guix-pcomplete-complete-command-arg (command)
  "Complete argument for guix COMMAND."
  (cond
   ((member command
            '("archive" "build" "challenge" "copy" "edit" "environment"
              "graph" "lint" "pack" "refresh" "size"))
    (while t
      (pcomplete-here (guix-package-names))))
   (t (pcomplete-here* (pcomplete-entries)))))

(defun guix-pcomplete-complete-option-arg (command option &optional input)
  "Complete argument for COMMAND's OPTION.
INPUT is the current partially completed string."
  (cl-flet ((option? (short long)
              (or (string= option short)
                  (string= option long)))
            (command? (&rest commands)
              (member command commands))
            (complete (entries)
              (pcomplete-here entries input nil t))
            (complete* (entries)
              (pcomplete-here* entries input t)))
    (cond
     ((option? "-L" "--load-path")
      (complete* (pcomplete-dirs)))
     ((string= "--key-download" option)
      (complete* guix-help-key-policies))

     ((command? "package")
      (cond
       ;; For '--install[=]' and '--remove[=]', try to complete a package
       ;; name (INPUT) after the "=" sign, and then the rest packages
       ;; separated with spaces.
       ((or (option? "-i" "--install")
            (option? "-r" "--remove"))
        (complete (guix-package-names))
        (while (not (guix-pcomplete-match-option))
          (pcomplete-here (guix-package-names))))
       ((string= "--show" option)
        (complete (guix-package-names)))
       ((option? "-p" "--profile")
        (complete* (pcomplete-dirs)))
       ((or (option? "-f" "--install-from-file")
            (option? "-m" "--manifest"))
        (complete* (pcomplete-entries)))))

     ((and (command? "archive" "build" "environment" "size")
           (option? "-s" "--system"))
      (complete* guix-help-system-types))

     ((and (command? "archive")
           (option? "-x" "--extract"))
      (complete* (pcomplete-dirs)))

     ((and (command? "build")
           (or (option? "-f" "--file")
               (option? "-r" "--root")
               (string= "--with-source" option)))
      (complete* (pcomplete-entries)))

     ((command? "graph")
      (cond
       ((option? "-t" "--type")
        (complete* (guix-graph-node-type-names)))
       ((option? "-b" "--backend")
        (complete* (guix-graph-backend-names)))))

     ((and (command? "environment")
           (option? "-l" "--load"))
      (complete* (pcomplete-entries)))

     ((and (command? "hash" "download")
           (option? "-f" "--format"))
      (complete* guix-help-hash-formats))

     ((and (command? "lint")
           (option? "-c" "--checkers"))
      (guix-pcomplete-complete-comma-args
       (guix-lint-checker-names)))

     ((command? "pack")
      (cond
       ((option? "-C" "--compression")
        (complete* (guix-compressor-names)))
       ((option? "-f" "--format")
        (complete* (guix-pack-format-names)))
       ;; Although the argument should be "FILE-NAME=TARGET", it is
       ;; still better to complete the FILE-NAME than to complete
       ;; nothing.
       ((option? "-S" "--symlink")
        (complete* (pcomplete-entries)))))

     ((command? "publish")
      (cond
       ((member option '("--public-key" "--private-key"))
        (complete* (pcomplete-entries)))
       ((option? "-u" "--user")
        (complete* (pcmpl-unix-user-names)))))

     ((command? "refresh")
      (cond
       ((option? "-s" "--select")
        (complete* guix-help-refresh-subsets))
       ((option? "-t" "--type")
        (guix-pcomplete-complete-comma-args
         (guix-refresh-updater-names)))))

     ((and (command? "size")
           (option? "-m" "--map-file"))
      (complete* (pcomplete-entries))))))

(defun guix-pcomplete-complete-options (command)
  "Complete options (with their arguments) for guix COMMAND."
  (while (guix-pcomplete-match-option)
    (let ((index pcomplete-index))
      (if (guix-pcomplete-match-long-option)

          ;; Long options.
          (if (guix-pcomplete-match-long-option-with-arg)
              (let ((option (pcomplete-match-string 1 0))
                    (arg    (pcomplete-match-string 2 0)))
                (guix-pcomplete-complete-option-arg
                 command option arg))

            (pcomplete-here* (guix-pcomplete-long-options command))
            ;; We support '--opt arg' style (along with '--opt=arg'),
            ;; because 'guix package --install/--remove' may be used this
            ;; way.  So try to complete an argument after the option has
            ;; been completed.
            ;;
            ;; XXX This leads to a problem: optional arguments cannot be
            ;; completed.  For example, after typing "guix build --sources ",
            ;; most likely, a user would want to complete a package name, so
            ;; we can't complete sources type there.
            (unless (guix-pcomplete-match-option)
              (guix-pcomplete-complete-option-arg
               command (pcomplete-arg 0 -1))))

        ;; Short options.
        (let ((arg (pcomplete-arg 0)))
          (if (> (length arg) 2)
              ;; Support specifying an argument after a short option without
              ;; spaces (for example, '-L/tmp/foo').
              (guix-pcomplete-complete-option-arg
               command
               (substring-no-properties arg 0 2)
               (substring-no-properties arg 2))
            (pcomplete-opt (guix-pcomplete-short-options command))
            (guix-pcomplete-complete-option-arg
             command (pcomplete-arg 0 -1)))))

      ;; If there were no completions, move to the next argument and get
      ;; out if the last argument is achieved.
      (when (= index pcomplete-index)
        (if (= pcomplete-index pcomplete-last)
            (throw 'pcompleted nil)
          (pcomplete-next-arg))))))

;;;###autoload
(defun pcomplete/guix ()
  "Completion for `guix'."
  (let ((commands (guix-pcomplete-commands)))
    (pcomplete-here* (cons "--help" commands))
    (let ((command (pcomplete-arg 'first 1)))
      (when (member command commands)
        (guix-pcomplete-complete-options command)
        (let ((subcommands (guix-pcomplete-commands command)))
          (when subcommands
            (pcomplete-here* subcommands)))
        (guix-pcomplete-complete-options command)
        (guix-pcomplete-complete-command-arg command)))))

(provide 'guix-pcomplete)

;;; guix-pcomplete.el ends here
