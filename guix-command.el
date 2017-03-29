;;; guix-command.el --- Popup interface for guix commands  -*- lexical-binding: t -*-

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

;; This file provides a magit-like popup interface for running guix
;; commands in Guix REPL.  The entry point is "M-x guix".  When it is
;; called the first time, "guix --help" output is parsed and
;; `guix-COMMAND-action' functions are generated for each available guix
;; COMMAND.  Then a window with these commands is popped up.  When a
;; particular COMMAND is called, "guix COMMAND --help" output is parsed,
;; and a user get a new popup window with available options for this
;; command and so on.

;; To avoid hard-coding all guix options, actions, etc., as much data is
;; taken from "guix ... --help" outputs as possible.  But this data is
;; still incomplete: not all long options have short analogs, also
;; special readers should be used for some options (for example, to
;; complete package names while prompting for a package).  So after
;; parsing --help output, the arguments are "improved".  All arguments
;; (switches, options and actions) are `guix-command-argument'
;; structures.

;; Only "M-x guix" command is available after this file is loaded.  The
;; rest commands/actions/popups are generated on the fly only when they
;; are needed (that's why there is a couple of `eval'-s in this file).

;; COMMANDS argument is used by many functions in this file.  It means a
;; list of guix commands without "guix" itself, e.g.: ("build"),
;; ("import" "gnu").  The empty list stands for the plain "guix" without
;; subcommands.

;; All actions in popup windows are divided into 2 groups:
;;
;; - 'Popup' actions - used to pop up another window.  For example, every
;;   action in the 'guix' or 'guix import' window is a popup action.  They
;;   are defined by `guix-command-define-popup-action' macro.
;;
;; - 'Execute' actions - used to do something with the command line (to
;;   run a command in Guix REPL or to copy it into kill-ring) constructed
;;   with the current popup.  They are defined by
;;   `guix-command-define-execute-action' macro.

;;; Code:

(require 'cl-lib)
(require 'bui-utils)
(require 'guix nil t)
(require 'guix-popup)
(require 'guix-utils)
(require 'guix-help-vars)
(require 'guix-read)
(require 'guix-misc)
(require 'guix-build-log)
(require 'guix-guile)
(require 'guix-external)

(defgroup guix-commands nil
  "Settings for guix popup windows."
  :group 'guix)

(defvar guix-command-complex-with-shared-arguments
  '("system")
  "List of guix commands which have subcommands with shared options.
I.e., 'guix foo --help' is the same as 'guix foo bar --help'.")

(defun guix-command-action-name (&optional commands &rest name-parts)
  "Return name of action function for guix COMMANDS."
  (guix-command-symbol (append commands name-parts (list "action"))))


;;; Command arguments

(cl-defstruct (guix-command-argument
               (:constructor guix-command-make-argument)
               (:copier      guix-command-copy-argument))
  name char doc fun switch? option? action?)

(cl-defun guix-command-modify-argument
    (argument &key
              (name    nil name-bound?)
              (char    nil char-bound?)
              (doc     nil doc-bound?)
              (fun     nil fun-bound?)
              (switch? nil switch?-bound?)
              (option? nil option?-bound?)
              (action? nil action?-bound?))
  "Return a modified version of ARGUMENT."
  (declare (indent 1))
  (let ((copy (guix-command-copy-argument argument)))
    (and name-bound?    (setf (guix-command-argument-name    copy) name))
    (and char-bound?    (setf (guix-command-argument-char    copy) char))
    (and doc-bound?     (setf (guix-command-argument-doc     copy) doc))
    (and fun-bound?     (setf (guix-command-argument-fun     copy) fun))
    (and switch?-bound? (setf (guix-command-argument-switch? copy) switch?))
    (and option?-bound? (setf (guix-command-argument-option? copy) option?))
    (and action?-bound? (setf (guix-command-argument-action? copy) action?))
    copy))

(defun guix-command-modify-argument-from-alist (argument alist)
  "Return a modified version of ARGUMENT or nil if it wasn't modified.
Each assoc from ALIST have a form (NAME . PLIST).  NAME is an
argument name.  PLIST is a property list of argument parameters
to be modified."
  (let* ((name  (guix-command-argument-name argument))
         (plist (bui-assoc-value alist name)))
    (when plist
      (apply #'guix-command-modify-argument
             argument plist))))

(defmacro guix-command-define-argument-improver (name alist)
  "Define NAME variable and function to modify an argument from ALIST."
  (declare (indent 1))
  `(progn
     (defvar ,name ,alist)
     (defun ,name (argument)
       (guix-command-modify-argument-from-alist argument ,name))))

(guix-command-define-argument-improver
    guix-command-improve-action-argument
  '(("container"   :char ?C)
    ("copy"        :char ?y)
    ("graph"       :char ?G)
    ("environment" :char ?E)
    ("pack"        :char ?k)
    ("publish"     :char ?u)
    ("pull"        :char ?P)
    ("size"        :char ?z)))

(guix-command-define-argument-improver
    guix-command-improve-common-argument
  '(("--help"    :switch? nil)
    ("--version" :switch? nil)))

(guix-command-define-argument-improver
    guix-command-improve-target-argument
  '(("--target" :char ?T)))

(guix-command-define-argument-improver
    guix-command-improve-system-type-argument
  '(("--system" :fun guix-read-system-type)))

(guix-command-define-argument-improver
    guix-command-improve-load-path-argument
  '(("--load-path" :fun read-directory-name)))

(guix-command-define-argument-improver
    guix-command-improve-search-paths-argument
  '(("--search-paths" :char ?P)))

(guix-command-define-argument-improver
    guix-command-improve-substitute-urls-argument
  '(("--substitute-urls" :char ?U)))

(guix-command-define-argument-improver
    guix-command-improve-hash-argument
  '(("--format" :fun guix-read-hash-format)))

(guix-command-define-argument-improver
    guix-command-improve-key-policy-argument
  '(("--key-download" :fun guix-read-key-policy)))

(defvar guix-command-improve-common-build-argument
  '(("--no-substitutes"  :char ?s)
    ("--no-build-hook"   :char ?h)
    ("--max-silent-time" :char ?X)
    ("--rounds"          :char ?R :fun read-number)
    ("--no-grafts"       :char ?G)
    ("--with-graft"      :char ?g)
    ("--with-input"      :char ?W)))

(defun guix-command-improve-common-build-argument (argument)
  (guix-command-modify-argument-from-alist
   argument
   (append guix-command-improve-load-path-argument
           guix-command-improve-substitute-urls-argument
           guix-command-improve-common-build-argument)))

(guix-command-define-argument-improver
    guix-command-improve-archive-argument
  '(("--extract" :fun read-directory-name)))

(guix-command-define-argument-improver
    guix-command-improve-build-argument
  '(("--file"        :fun guix-read-file-name)
    ("--root"        :fun guix-read-file-name)
    ("--sources"     :char ?S :fun guix-read-source-type :switch? nil)
    ("--with-source" :fun guix-read-file-name)))

(guix-command-define-argument-improver
    guix-command-improve-copy-argument
  '(("--to" :char ?T)))

(guix-command-define-argument-improver
    guix-command-improve-environment-argument
  '(("--ad-hoc"
     :name "--ad-hoc " :fun guix-read-package-names-string
     :switch? nil :option? t)
    ("--expose" :char ?E)
    ("--share" :char ?S)
    ("--load" :fun guix-read-file-name)))

(guix-command-define-argument-improver
    guix-command-improve-gc-argument
  '(("--list-dead" :char ?D)
    ("--list-live" :char ?L)
    ("--referrers" :char ?f)
    ("--verify"    :fun guix-read-verify-options-string)))

(guix-command-define-argument-improver
    guix-command-improve-graph-argument
  '(("--list-backends" :char ?b)
    ("--list-types" :char ?t)
    ("--backend" :fun guix-read-graph-backend)
    ("--type" :fun guix-read-graph-node-type)))

(guix-command-define-argument-improver
    guix-command-improve-import-argument
  '(("gem"   :char ?G)
    ("crate" :char ?C)
    ("cran"  :char ?r)))

(guix-command-define-argument-improver
    guix-command-improve-import-elpa-argument
  '(("--archive" :fun guix-read-elpa-archive)))

(guix-command-define-argument-improver
    guix-command-improve-lint-argument
  '(("--checkers" :fun guix-read-lint-checker-names-string)))

(guix-command-define-argument-improver
    guix-command-improve-pack-argument
  '(("--compression" :fun guix-read-compressor-name)
    ("--format" :fun guix-read-pack-format-name)
    ;; "--symlink" is not completed as it should be "FILE-NAME=TARGET".
    ;; ("--symlink" :fun guix-read-file-name)
    ))

(guix-command-define-argument-improver
    guix-command-improve-package-argument
  ;; Unlike all other options, --install/--remove do not have a form
  ;; '--install=foo,bar' but '--install foo bar' instead, so we need
  ;; some tweaks.
  '(("--install"
     :name "--install " :fun guix-read-package-names-string
     :switch? nil :option? t)
    ("--remove"
     :name "--remove "  :fun guix-read-package-names-string
     :switch? nil :option? t)
    ("--install-from-file" :fun guix-read-file-name)
    ("--manifest"       :fun guix-read-file-name)
    ("--profile"        :fun guix-read-file-name)
    ;; Although it is documented that regexp is optional for --upgrade
    ;; and --do-not-upgrade, use them only as options (not as switches).
    ("--upgrade"        :switch? nil)
    ("--do-not-upgrade" :char ?n :switch? nil)
    ("--roll-back"      :char ?R)
    ("--show"           :char ?h :fun guix-read-package-name)))

(guix-command-define-argument-improver
    guix-command-improve-publish-argument
  '(("--public-key"  :char ?k :fun guix-read-file-name)
    ("--private-key" :char ?K :fun guix-read-file-name)
    ("--user" :fun guix-read-user-name)))

(guix-command-define-argument-improver
    guix-command-improve-refresh-argument
  '(("--select"     :fun guix-read-refresh-subset)
    ("--type"       :fun guix-read-refresh-updater-names-string)
    ("--key-server" :char ?S)))

(guix-command-define-argument-improver
    guix-command-improve-size-argument
  '(("--map-file" :fun guix-read-file-name)))

(guix-command-define-argument-improver
    guix-command-improve-system-argument
  '(("disk-image"  :char ?D)
    ("vm-image"    :char ?V)
    ("roll-back"   :char ?R)
    ("switch-generation" :char ?S)
    ("--on-error"  :char ?E)
    ("--no-grub"   :char ?g)
    ("--full-boot" :char ?b)))

(defvar guix-command-argument-improvers
  '((()
     guix-command-improve-action-argument)
    (("archive")
     guix-command-improve-common-build-argument
     guix-command-improve-target-argument
     guix-command-improve-system-type-argument
     guix-command-improve-archive-argument)
    (("build")
     guix-command-improve-common-build-argument
     guix-command-improve-target-argument
     guix-command-improve-system-type-argument
     guix-command-improve-build-argument)
    (("copy")
     guix-command-improve-common-build-argument
     guix-command-improve-copy-argument)
    (("download")
     guix-command-improve-hash-argument)
    (("hash")
     guix-command-improve-hash-argument)
    (("environment")
     guix-command-improve-common-build-argument
     guix-command-improve-search-paths-argument
     guix-command-improve-system-type-argument
     guix-command-improve-environment-argument)
    (("gc")
     guix-command-improve-gc-argument)
    (("graph")
     guix-command-improve-graph-argument)
    (("import")
     guix-command-improve-import-argument)
    (("import" "gnu")
     guix-command-improve-key-policy-argument)
    (("import" "elpa")
     guix-command-improve-import-elpa-argument)
    (("lint")
     guix-command-improve-lint-argument)
    (("pack")
     guix-command-improve-common-build-argument
     guix-command-improve-system-type-argument
     guix-command-improve-target-argument
     guix-command-improve-pack-argument)
    (("package")
     guix-command-improve-common-build-argument
     guix-command-improve-search-paths-argument
     guix-command-improve-package-argument)
    (("publish")
     guix-command-improve-publish-argument)
    (("refresh")
     guix-command-improve-key-policy-argument
     guix-command-improve-refresh-argument)
    (("size")
     guix-command-improve-system-type-argument
     guix-command-improve-substitute-urls-argument
     guix-command-improve-size-argument)
    (("system")
     guix-command-improve-common-build-argument
     guix-command-improve-system-argument))
  "Alist of guix commands and argument improvers for them.")

(defun guix-command-improve-argument (argument improvers)
  "Return ARGUMENT modified with IMPROVERS."
  (or (cl-some (lambda (improver)
                 (funcall improver argument))
               improvers)
      argument))

(defun guix-command-improve-arguments (arguments commands)
  "Return ARGUMENTS for 'guix COMMANDS ...' modified for popup interface."
  (let ((improvers (cons 'guix-command-improve-common-argument
                         (bui-assoc-value guix-command-argument-improvers
                                           commands))))
    (mapcar (lambda (argument)
              (guix-command-improve-argument argument improvers))
            arguments)))

(defun guix-command-parse-arguments (&optional commands)
  "Return a list of parsed 'guix COMMANDS ...' arguments."
  (with-temp-buffer
    (insert (guix-help-string commands))
    (let (args)
      (guix-while-search guix-help-parse-option-regexp
        (let* ((short (match-string-no-properties 1))
               (name  (match-string-no-properties 2))
               (arg   (match-string-no-properties 3))
               (doc   (match-string-no-properties 4))
               (char  (if short
                          (elt short 1) ; short option letter
                        (elt name 2))) ; first letter of the long option
               ;; If "--foo=bar" or "--foo[=bar]" then it is 'option'.
               (option? (not (string= "" arg)))
               ;; If "--foo" or "--foo[=bar]" then it is 'switch'.
               (switch? (or (string= "" arg)
                            (eq ?\[ (elt arg 0)))))
          (push (guix-command-make-argument
                 :name    name
                 :char    char
                 :doc     doc
                 :switch? switch?
                 :option? option?)
                args)))
      (guix-while-search guix-help-parse-command-regexp
        (let* ((name (match-string-no-properties 1))
               (char (elt name 0)))
          (push (guix-command-make-argument
                 :name    name
                 :char    char
                 :fun     (guix-command-action-name commands name)
                 :action? t)
                args)))
      args)))

(defun guix-command-rest-argument (&optional commands)
  "Return '--' argument for COMMANDS."
  (cl-flet ((argument (&rest args)
              (apply #'guix-command-make-argument
                     :name "-- " :char ?= :option? t args)))
    (let ((command (car commands)))
      (cond
       ((member command
                '("archive" "build" "challenge" "copy" "edit"
                  "graph" "lint" "pack" "refresh"))
        (argument :doc "Packages" :fun 'guix-read-package-names-string))
       ((equal commands '("container" "exec"))
        (argument :doc "PID Command [Args...]"))
       ((string= command "download")
        (argument :doc "URL"))
       ((string= command "environment")
        (argument :doc "Command [Args...]" :fun 'read-shell-command))
       ((string= command "gc")
        (argument :doc "Paths" :fun 'guix-read-file-name))
       ((member command '("hash" "system"))
        (argument :doc "File" :fun 'guix-read-file-name))
       ((string= command "size")
        (argument :doc "Package" :fun 'guix-read-package-name))
       ((equal commands '("import" "nix"))
        (argument :doc "Nixpkgs Attribute"))
       ;; Other 'guix import' subcommands, but not 'import' itself.
       ((and (cdr commands)
             (string= command "import"))
        (argument :doc "Package name"))))))

(defvar guix-command-additional-arguments
  `((("environment")
     ,(guix-command-make-argument
       :name "++packages " :char ?p :option? t
       :doc "build inputs of the specified packages"
       :fun 'guix-read-package-names-string)))
  "Alist of guix commands and additional arguments for them.
These are 'fake' arguments that are not presented in 'guix' shell
commands.")

(defun guix-command-additional-arguments (&optional commands)
  "Return additional arguments for COMMANDS."
  (let ((rest-arg (guix-command-rest-argument commands)))
    (append (bui-assoc-value guix-command-additional-arguments
                              commands)
            (and rest-arg (list rest-arg)))))

;; Ideally only `guix-command-arguments' function should exist with the
;; contents of `guix-command-all-arguments', but we need to make a
;; special case for `guix-command-complex-with-shared-arguments' commands.

(defun guix-command-all-arguments (&optional commands)
  "Return list of all arguments for 'guix COMMANDS ...'."
  (let ((parsed (guix-command-parse-arguments commands)))
    (append (guix-command-improve-arguments parsed commands)
            (guix-command-additional-arguments commands))))

(guix-memoized-defalias guix-command-all-arguments-memoize
  guix-command-all-arguments)

(defun guix-command-arguments (&optional commands)
  "Return list of arguments for 'guix COMMANDS ...'."
  (let ((command (car commands)))
    (if (member command
                guix-command-complex-with-shared-arguments)
        ;; Take actions only for 'guix system', and switches+options for
        ;; 'guix system foo'.
        (funcall (if (null (cdr commands))
                     #'cl-remove-if-not
                   #'cl-remove-if)
                 #'guix-command-argument-action?
                 (guix-command-all-arguments-memoize (list command)))
      (guix-command-all-arguments commands))))

(defun guix-command-switch->popup-switch (switch)
  "Return popup switch from command SWITCH argument."
  (list (guix-command-argument-char switch)
        (or (guix-command-argument-doc switch)
            "Unknown")
        (guix-command-argument-name switch)))

(defun guix-command-option->popup-option (option)
  "Return popup option from command OPTION argument."
  (list (guix-command-argument-char option)
        (or (guix-command-argument-doc option)
            "Unknown")
        (let ((name (guix-command-argument-name option)))
          (if (string-match-p " \\'" name) ; ends with space
              name
            (concat name "=")))
        (or (guix-command-argument-fun option)
            'read-from-minibuffer)))

(defun guix-command-action->popup-action (action)
  "Return popup action from command ACTION argument."
  (list (guix-command-argument-char action)
        (or (guix-command-argument-doc action)
            (guix-command-argument-name action)
            "Unknown")
        (guix-command-argument-fun action)))

(defun guix-command-sort-arguments (arguments)
  "Sort ARGUMENTS by name in alphabetical order."
  (sort arguments
        (lambda (a1 a2)
          (let ((name1 (guix-command-argument-name a1))
                (name2 (guix-command-argument-name a2)))
            (cond ((null name1) nil)
                  ((null name2) t)
                  (t (string< name1 name2)))))))

(defun guix-command-switches (arguments)
  "Return switches from ARGUMENTS."
  (cl-remove-if-not #'guix-command-argument-switch? arguments))

(defun guix-command-options (arguments)
  "Return options from ARGUMENTS."
  (cl-remove-if-not #'guix-command-argument-option? arguments))

(defun guix-command-actions (arguments)
  "Return actions from ARGUMENTS."
  (cl-remove-if-not #'guix-command-argument-action? arguments))


;;; Post processing popup arguments

(defvar guix-command-post-processors
  '(("environment"
     guix-command-post-process-environment-packages
     guix-command-post-process-environment-ad-hoc
     guix-command-post-process-rest-multiple-leave)
    ("hash"
     guix-command-post-process-rest-single)
    ("package"
     guix-command-post-process-package-args)
    ("system"
     guix-command-post-process-rest-single))
  "Alist of guix commands and functions for post-processing
a list of arguments returned from popup interface.
Each function is called on the returned arguments in turn.")

(defvar guix-command-rest-arg-regexp
  (rx string-start "-- " (group (+ any)))
  "Regexp to match a string with the 'rest' arguments.")

(defun guix-command-replace-args (args predicate modifier)
  "Replace arguments matching PREDICATE from ARGS.
Call MODIFIER on each argument matching PREDICATE and append the
returned list of strings to the end of ARGS.  Remove the original
arguments."
  (let* ((rest nil)
         (args (mapcar (lambda (arg)
                         (if (funcall predicate arg)
                             (progn
                               (push (funcall modifier arg) rest)
                               nil)
                           arg))
                       args)))
    (if rest
        (apply #'append (delq nil args) rest)
      args)))

(cl-defun guix-command-post-process-matching-args (args regexp
                                                   &key group split?)
  "Modify arguments from ARGS matching REGEXP by moving them to
the end of ARGS list.  If SPLIT? is non-nil, split matching
arguments into multiple subarguments."
  (guix-command-replace-args
   args
   (lambda (arg)
     (string-match regexp arg))
   (lambda (arg)
     (let ((val (match-string (or group 0) arg))
           (fun (if split? #'split-string #'list)))
       (funcall fun val)))))

(defun guix-command-post-process-rest-single (args)
  "Modify ARGS by moving '-- ARG' argument to the end of ARGS list."
  (guix-command-post-process-matching-args
   args guix-command-rest-arg-regexp
   :group 1))

(defun guix-command-post-process-rest-multiple (args)
  "Modify ARGS by splitting '-- ARG ...' into multiple subarguments
and moving them to the end of ARGS list.
Remove '-- ' string."
  (guix-command-post-process-matching-args
   args guix-command-rest-arg-regexp
   :group 1
   :split? t))

(defun guix-command-post-process-rest-multiple-leave (args)
  "Modify ARGS by splitting '-- ARG ...' into multiple subarguments
and moving them to the end of ARGS list.
Leave '--' string as a separate argument."
  (guix-command-post-process-matching-args
   args guix-command-rest-arg-regexp
   :split? t))

(defun guix-command-post-process-package-args (args)
  "Adjust popup ARGS for 'guix package' command."
  (guix-command-post-process-matching-args
   args (rx string-start (or "--install " "--remove ") (+ any))
   :split? t))

(defun guix-command-post-process-environment-packages (args)
  "Adjust popup ARGS for specified packages of 'guix environment'
command."
  (guix-command-post-process-matching-args
   args (rx string-start "++packages " (group (+ any)))
   :group 1
   :split? t))

(defun guix-command-post-process-environment-ad-hoc (args)
  "Adjust popup ARGS for '--ad-hoc' argument of 'guix environment'
command."
  (guix-command-post-process-matching-args
   args (rx string-start "--ad-hoc " (+ any))
   :split? t))

(defun guix-command-post-process-args (commands args)
  "Adjust popup ARGS for guix COMMANDS."
  (let* ((command (car commands))
         (processors
          (append (bui-assoc-value guix-command-post-processors commands)
                  (bui-assoc-value guix-command-post-processors command))))
    (apply #'guix-modify
           args
           (or processors
               (list #'guix-command-post-process-rest-multiple)))))


;;; 'Execute' actions

(defvar guix-command-default-execute-arguments
  (list
   (guix-command-make-argument
    :name "repl"  :char ?r :doc "Run in Guix REPL")
   (guix-command-make-argument
    :name "shell" :char ?s :doc "Run in shell")
   (guix-command-make-argument
    :name "copy"  :char ?c :doc "Copy command line"))
  "List of default 'execute' action arguments.")

(defvar guix-command-additional-execute-arguments
  (let ((graph-arg (guix-command-make-argument
                    :name "view" :char ?v :doc "View graph")))
    `((("build")
       ,(guix-command-make-argument
         :name "log" :char ?l :doc "View build log"))
      (("graph") ,graph-arg)
      (("size")
       ,(guix-command-make-argument
         :name "view" :char ?v :doc "View map"))
      (("system" "shepherd-graph") ,graph-arg)
      (("system" "extension-graph") ,graph-arg)))
  "Alist of guix commands and additional 'execute' action arguments.")

(defun guix-command-execute-arguments (commands)
  "Return a list of 'execute' action arguments for COMMANDS."
  (mapcar (lambda (arg)
            (guix-command-modify-argument arg
              :action? t
              :fun (guix-command-action-name
                    commands (guix-command-argument-name arg))))
          (append guix-command-default-execute-arguments
                  (bui-assoc-value
                   guix-command-additional-execute-arguments commands))))

(defvar guix-command-special-executors
  '((("environment")
     ("repl" . guix-run-environment-command-in-repl))
    (("pull")
     ("repl" . guix-run-pull-command-in-repl))
    (("build")
     ("log" . guix-run-view-build-log))
    (("graph")
     ("view" . guix-run-view-graph))
    (("size")
     ("view" . guix-run-view-size-map))
    (("system" "shepherd-graph")
     ("view" . guix-run-view-graph))
    (("system" "extension-graph")
     ("view" . guix-run-view-graph)))
  "Alist of guix commands and alists of special executers for them.
See also `guix-command-default-executors'.")

(defvar guix-command-default-executors
  '(("repl"  . guix-run-command-in-repl)
    ("shell" . guix-run-command-in-shell)
    ("copy"  . guix-copy-command-as-kill))
  "Alist of default executers for action names.")

(defun guix-command-executor (commands name)
  "Return function to run command line arguments for guix COMMANDS."
  (or (bui-assoc-value guix-command-special-executors commands name)
      (bui-assoc-value guix-command-default-executors name)))

(defun guix-run-environment-command-in-repl (args)
  "Run 'guix ARGS ...' environment command in Guix REPL."
  ;; As 'guix environment' usually tries to run another process, it may
  ;; be fun but not wise to run this command in Geiser REPL.
  (when (or (member "--dry-run" args)
            (member "--search-paths" args)
            (when (y-or-n-p
                   (format "'%s' command will spawn an external process.
Do you really want to execute this command in Geiser REPL? "
                           (guix-command-string args)))
              (message (substitute-command-keys
                        "May \"\\[shell-mode]\" be with you!"))
              t))
    (guix-run-command-in-repl args)))

(defun guix-run-pull-command-in-repl (args)
  "Run 'guix ARGS ...' pull command in Guix REPL.
Perform pull-specific actions after operation, see
`guix-after-pull-hook' and `guix-update-after-pull'."
  (guix-eval-in-repl
   (apply #'guix-make-guile-expression 'guix-command args)
   nil 'pull))

(defun guix-run-view-build-log (args)
  "Add --log-file to ARGS, run 'guix ARGS ...' build command, and
open the log file(s)."
  (let* ((args (if (member "--log-file" args)
                   args
                 (cl-list* (car args) "--log-file" (cdr args))))
         (output (guix-command-output args))
         (files  (split-string output "\n" t)))
    (dolist (file files)
      (guix-build-log-find-file file))))

(declare-function guix-make-view-graph "guix-graph" t)

(defun guix-run-view-graph (args)
  "Run 'guix ARGS ...' graph command, make the image and open it."
  (require 'guix-graph)
  (guix-make-view-graph
   (if (member "--backend=d3js" args) "d3js" "graphviz")
   (lambda (graph-type graph-file)
     (guix-eval-read
      (cl-case graph-type
        (dot  (guix-make-guile-expression
               'pipe-guix-output
               args (guix-dot-arguments graph-file)))
        (html (guix-make-guile-expression
               'guix-output-to-file args graph-file)))))))

(defun guix-run-view-size-map (args)
  "Run 'guix ARGS ...' size command, and open the map file."
  (let* ((wished-map-file
          (cl-some (lambda (arg)
                     (and (string-match "--map-file=\\(.+\\)" arg)
                          (match-string 1 arg)))
                   args))
         (map-file (or wished-map-file (guix-png-file-name)))
         (args (if wished-map-file
                   args
                 (cl-list* (car args)
                           (concat "--map-file=" map-file)
                           (cdr args)))))
    (guix-command-output args)
    (guix-find-file map-file)))


;;; Generating popups, actions, etc.

(defmacro guix-command-define-popup-action (name &optional commands)
  "Define NAME function to generate (if needed) and run popup for COMMANDS."
  (declare (indent 1) (debug t))
  (let* ((popup-fun (guix-command-symbol `(,@commands "popup")))
         (doc (format "Call `%s' (generate it if needed)."
                      popup-fun)))
    `(defun ,name (&optional arg)
       ,doc
       (interactive "P")
       (unless (fboundp ',popup-fun)
         (guix-command-generate-popup ',popup-fun ',commands))
       (,popup-fun arg))))

(defmacro guix-command-define-execute-action (name executor
                                                   &optional commands)
  "Define NAME function to execute the current action for guix COMMANDS.
EXECUTOR function is called with the current command line arguments."
  (declare (indent 1) (debug t))
  (let* ((arguments-fun (guix-command-symbol `(,@commands "arguments")))
         (doc (format "Call `%s' with the current popup arguments."
                      executor)))
    `(defun ,name (&rest args)
       ,doc
       (interactive (,arguments-fun))
       (,executor (append ',commands
                          (guix-command-post-process-args
                           ',commands args))))))

(defun guix-command-generate-popup-actions (actions &optional commands)
  "Generate 'popup' commands from ACTIONS arguments for guix COMMANDS."
  (dolist (action actions)
    (let ((fun (guix-command-argument-fun action)))
      (unless (fboundp fun)
        (eval `(guix-command-define-popup-action ,fun
                 ,(append commands
                          (list (guix-command-argument-name action)))))))))

(defun guix-command-generate-execute-actions (actions &optional commands)
  "Generate 'execute' commands from ACTIONS arguments for guix COMMANDS."
  (dolist (action actions)
    (let ((fun (guix-command-argument-fun action)))
      (unless (fboundp fun)
        (eval `(guix-command-define-execute-action ,fun
                 ,(guix-command-executor
                   commands (guix-command-argument-name action))
                 ,commands))))))

(defun guix-command-generate-popup (name &optional commands)
  "Define NAME popup with 'guix COMMANDS ...' interface."
  (let* ((command  (car commands))
         (man-page (concat "guix" (and command (concat "-" command))))
         (doc      (format "Popup window for '%s' command."
                           (guix-concat-strings (cons "guix" commands)
                                                " ")))
         (args     (guix-command-arguments commands))
         (switches (guix-command-sort-arguments
                    (guix-command-switches args)))
         (options  (guix-command-sort-arguments
                    (guix-command-options args)))
         (popup-actions (guix-command-sort-arguments
                         (guix-command-actions args)))
         (execute-actions (unless popup-actions
                            (guix-command-execute-arguments commands)))
         (actions (or popup-actions execute-actions)))
    (if popup-actions
        (guix-command-generate-popup-actions popup-actions commands)
      (guix-command-generate-execute-actions execute-actions commands))
    (eval
     `(guix-define-popup ,name
        ,doc
        'guix-commands
        :man-page ,man-page
        :switches ',(mapcar #'guix-command-switch->popup-switch switches)
        :options  ',(mapcar #'guix-command-option->popup-option options)
        :actions  ',(mapcar #'guix-command-action->popup-action actions)
        :max-action-columns 4))))

(declare-function guix-popup "guix-command" t)

;;;###autoload (autoload 'guix "guix-command" "Popup window for 'guix'." t)
(guix-command-define-popup-action guix)

(declare-function guix-edit "guix-location" t)

(defalias 'guix-edit-action #'guix-edit)


(defvar guix-command-font-lock-keywords
  (eval-when-compile
    `((,(rx "("
            (group "guix-command-define-"
                   (or "popup-action"
                       "execute-action"
                       "argument-improver"))
            symbol-end
            (zero-or-more blank)
            (zero-or-one
             (group (one-or-more (or (syntax word) (syntax symbol))))))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode guix-command-font-lock-keywords)

(provide 'guix-command)

;;; guix-command.el ends here
