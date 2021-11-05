;;; nix-flake.el --- Transient interface to Nix flake commands -*- lexical-binding: t -*-

;;; Commentary:

;; This library provides a transient interface to flake commands.

;; This is experimental.

;;; Code:

(require 'transient)
(require 'nix)

(defvar nix-flake-ref nil)

(defvar nix-flake-outputs nil)

(defclass nix-flake:update-input (transient-option)
  ())

(transient-define-infix nix-flake-arg:update-input ()
  :class 'nix-flake:update-input
  :argument "--update-input"
  :reader 'nix-flake--read-input-path
  :prompt "Input: "
  :description "Update a specific flake path")

(cl-defmethod transient-format-value ((obj nix-flake:update-input))
  "Format --update-input arguments from OBJ."
  (let ((value (oref obj value)))
    (propertize (concat (oref obj argument)
                        (when value
                          (concat " " value)))
                'face (if value
                          'transient-value
                        'transient-inactive-value))))

(cl-defmethod transient-infix-value ((obj nix-flake:update-input))
  "Return the value of the suffix object OBJ."
  (when-let ((value (oref obj value)))
    (list (oref obj argument) value)))

(defun nix-flake-system-attribute-names (types)
  "Return a list of output attributes of particular TYPES."
  (let ((system (intern (nix-system))))
    (thread-last nix-flake-outputs
      (mapcar (pcase-lambda (`(,type . ,alist))
		(when (memq type types)
		  (mapcar #'car (alist-get system alist)))))
      (apply #'append)
      (cl-remove-duplicates)
      (mapcar #'symbol-name))))

(defun nix-flake--input-names ()
  "Return a list of inputs to the flake."
  (thread-last (nix--process-json "flake" "info" nix-flake-ref "--json")
    (alist-get 'locks)
    (alist-get 'nodes)
    (alist-get 'root)
    (alist-get 'inputs)
    (mapcar #'cdr)))

(defun nix-flake--read-input-path (prompt initial-input _history)
  "Read an input name of a flake from the user.

FIXME: PROMPT INITIAL-INPUT"
  (completing-read prompt (nix-flake--input-names)
		   nil nil initial-input))

(defun nix-flake--args ()
  "Return arguments for Nix command."
  (flatten-list (transient-args 'nix-flake-dispatch)))

(defun nix-flake--run-attribute-names ()
  "Return possible attribute names for run command."
  (nix-flake-system-attribute-names '(apps packages)))

(defun nix-flake--build-attribute-names ()
  "Return possible attribute names for build command."
  (nix-flake-system-attribute-names '(packages)))

(defun nix-flake--default-run-p ()
  "Return non-nil if there is the default derivation for run command."
  (not (null (nix-flake-system-attribute-names '(defaultApp defaultPackage)))))

(defun nix-flake--default-build-p ()
  "Return non-nil if there is the default derivation for build command."
  (not (null (nix-flake-system-attribute-names '(defaultPackage)))))

(defun nix-flake--to-list (x)
  "If X is not a list, make a singleton list containing it."
  (if (listp x)
      x
    (list x)))

(defun nix-flake--command (subcommand nix-args flake-ref &optional args)
  "Build a command line for a Nix subcommand.

SUBCOMMAND is a string or a list of strings which is a subcommand of Nix.

NIX-ARGS is a list of string passed directly after the
subcommand, before FLAKE-REF. ARGS is extra arguments to the
command after the flake reference."
  (concat nix-executable
	  " "
          (mapconcat #'shell-quote-argument
                     `(,@(nix-flake--to-list subcommand)
		       ,@nix-args
		       ,flake-ref)
                     " ")
          (if args
              (concat " -- " args)
            "")))

(defun nix-flake--installable-command (subcommand nix-args flake-ref attribute
						  &optional args)
  "Build a command line for a Nix subcommand.

This is like `nix-flake--command', but for a subcommand which
takes an installable as an argument. See the user manual of Nix
for what installable means.

SUBCOMMAND, NIX-ARGS, and FLAKE-REF are the same as in the
function. ATTRIBUTE is the name of a package, app, or anything
that refers to a derivation in the flake. It must be a string
that is concatenated with the sharp symbol in the installable
reference. ARGS is a list of strings passed to the Nix command
after \"--\". Note that some commands such as \"nix build\" do
not take the extra arguments."
  (concat nix-executable
	  " "
          (mapconcat #'shell-quote-argument
                     `(,@(nix-flake--to-list subcommand)
		       ,@nix-args
		       ,(if attribute
			    (concat flake-ref "#" attribute)
			  flake-ref))
                     " ")
          (if args
              (concat " -- " args)
            "")))

(defun nix-flake-run-attribute (args flake-ref attribute command-args)
  "Run an app in the current flake.

ARGS and FLAKE-REF are the same as in other Nix commands.
ATTRIBUTE is the name of a package or app in the flake, and
COMMAND-ARGS is an optional list of strings passed to the
application."
  (interactive (list (nix-flake--args)
                     nix-flake-ref
                     (completing-read "Nix app/package: "
                                      (nix-flake--run-attribute-names))
                     nil))
  (compile (nix-flake--installable-command "run" args flake-ref attribute
                                           command-args)))

(defun nix-flake-run-default (args flake-ref command-args)
  "Run the default app or package in the current flake.

For ARGS, FLAKE-REF, and COMMAND-ARGS, see the documentation of
`nix-flake-run-attribute'."
  (interactive (list (nix-flake--args)
                     nix-flake-ref
                     nil))
  (compile (nix-flake--installable-command "run" args flake-ref nil
                                           command-args)))

(defun nix-flake-build-attribute (args flake-ref attribute)
  "Build a derivation in the current flake.

For ARGS, FLAKE-REF, and ATTRIBUTE, see the documentation of
`nix-flake-run-attribute'."
  (interactive (list (nix-flake--args)
                     nix-flake-ref
                     (completing-read "Nix package: "
                                      (nix-flake--build-attribute-names))))
  (compile (nix-flake--installable-command "build" args flake-ref attribute)))

(defun nix-flake-build-default (args flake-ref)
  "Build the default package in the current flake.


For ARGS and FLAKE-REF, see the documentation of
`nix-flake-run-attribute'."
  (interactive (list (nix-flake--args)
                     nix-flake-ref))
  (compile (nix-flake--installable-command "build" args flake-ref nil)))

(defun nix-flake-check (args flake-ref)
  "Check the flake.

For ARGS and FLAKE-REF, see the documentation of
`nix-flake-run-attribute'."
  (interactive (list (nix-flake--args) nix-flake-ref))
  (compile (nix-flake--command '("flake" "check") args flake-ref)))

(defun nix-flake-lock (args flake-ref)
  "Create missing lock file entries.

For ARGS and FLAKE-REF, see the documentation of
`nix-flake-run-attribute'."
  (interactive (list (nix-flake--args) nix-flake-ref))
  (compile (nix-flake--command '("flake" "lock") args flake-ref)))

(defun nix-flake-update (args flake-ref)
  "Update the lock file.

For ARGS and FLAKE-REF, see the documentation of
`nix-flake-run-attribute'."
  (interactive (list (nix-flake--args) nix-flake-ref))
  (compile (nix-flake--command '("flake" "update") args flake-ref)))

(defun nix-flake--description ()
  "Describe the current flake."
  (concat "Flake: " nix-flake-ref))

;;;###autoload (autoload 'nix-flake-dispatch "nix-flake" nil t)
(transient-define-prefix nix-flake-dispatch (flake-ref &optional remote)
  "Run a command on a Nix flake."
  ;; TODO: Allow switching the flake
  [:description nix-flake--description]
  ["Arguments"
   ("-m" "Allow access to mutable paths and repositories" "--impure")
   ("-u" nix-flake-arg:update-input)
   ("-nu" "Do not allow any updates to the flake's lock file" "--no-update-lock-file")
   ("-c" "Commit changes to the flake's lock file" "--commit-lock-file")]
  ["Installable commands"
   ("r" "Run attribute" nix-flake-run-attribute)
   ("R" "Run default" nix-flake-run-default :if nix-flake--default-run-p)
   ("b" "Build attribute" nix-flake-build-attribute)
   ("B" "Build default" nix-flake-build-default :if nix-flake--default-build-p)]
  ["Flake commands"
   ("c" "flake check" nix-flake-check)
   ("l" "flake lock" nix-flake-lock)
   ("u" "flake update" nix-flake-update)]
  (interactive (list (convert-standard-filename default-directory)))
  (setq nix-flake-ref flake-ref)
  (setq nix-flake-outputs
        (if remote
            (nix--process-json "flake" "show" "--json" nix-flake-ref)
          (let ((default-directory flake-ref))
	    (nix--process-json "flake" "show" "--json" "--no-update-lock-file"))))
  (transient-setup 'nix-flake-dispatch))

(defun nix-flake--registry-list ()
  "Return a list of entries from the registry."
  (cl-flet
      ((split-entry
        (s)
        (split-string s "[[:space:]]+")))
    (thread-last (nix--process-lines "registry" "list")
      (mapcar #'split-entry))))

(defun nix-flake--select-flake ()
  "Select a flake from the registry."
  (completing-read "Flake URL: "
                   (thread-last (nix-flake--registry-list)
                     (cl-remove-if-not (pcase-lambda (`(,type . ,_))
					 (member type '("user" "global"))))
                     (mapcar (pcase-lambda (`(,_ ,_ ,ref))
			       ref)))))

(defun nix-flake--directory-ref (dir)
  "Return the flake ref for a local DIR."
  (expand-file-name dir))

;; A wrapper function for ensuring existence of flake.nix and flake.lock
;; in the project directory.
(cl-defun nix-flake (dir &key flake-ref)
  "Dispatch a transient interface for Nix commands.

DIR is a directory on the file system in which flake.nix resides.

Alternatively, you can specify FLAKE-REF which follows the syntax
of flake-url. It can refer to a remote url, a local file path, or
whatever supported by Nix."
  (interactive (if (consp current-prefix-arg)
                   (list nil :flake-ref (nix-flake--select-flake))
                 (list default-directory)))
  (cl-assert (or (file-directory-p dir)
                 flake-ref)
             nil
             "DIR or FLAKE-REF must be specified")
  (cond
   (flake-ref
    (nix-flake-dispatch flake-ref t))
   ((file-exists-p (expand-file-name "flake.lock" dir))
    (nix-flake-dispatch (nix-flake--directory-ref dir)))
   ((file-exists-p (expand-file-name "flake.nix" dir))
    (message "You have not created flake.lock yet, so creating it...")
    (let ((default-directory dir))
      (nix-flake--command '("flake" "lock") nil
                          (nix-flake--directory-ref dir))))
   (t
    ;; TODO: Let the user run 'nix flake init' to create flake.nix
    (user-error "The directory does not contain flake.nix"))))

;;;; nix flake init

(defvar nix-flake-template-repository nil)

(defun nix-flake--init-source ()
  "Describe the current template repository for init command."
  (format "Template repository: %s" nix-flake-template-repository))

(defun nix-flake--templates (flake-ref)
  "Return a list of templates in FLAKE-REF."
  (thread-last (nix--process-json "flake" "show" "--json" flake-ref)
    (alist-get 'templates)
    (mapcar #'car)
    (mapcar #'symbol-name)))

(defun nix-flake--init (flake-ref template-name)
  "Initialize a flake from a template.

FLAKE-REF must be a reference to a flake which contains the
template, TEMPLATE-NAME is the name of the template."
  (compile (mapconcat #'shell-quote-argument
                      `(,nix-executable
                        "flake"
                        "init"
                        "-t"
                        ,(concat flake-ref "#" template-name))
		      " ")))

;; It might be better to use `transient-define-suffix', but I don't know for
;; sure.
(defun nix-flake-init-select-template ()
  "Select a template and initialize a flake."
  (interactive)
  (let* ((flake-ref (or nix-flake-template-repository
                        (nix-flake--select-flake)))
         (template-name (completing-read
                         (format-message "Select a template from %s: " flake-ref)
                         (nix-flake--templates flake-ref))))
    (nix-flake--init flake-ref template-name)))

;;;###autoload (autoload 'nix-flake-init "nix-flake" nil t)
(transient-define-prefix nix-flake-init-dispatch (&optional flake-ref)
  "Scaffold a project from a template."
  [:description "Initialize a flake"]
  [:description
   nix-flake--init-source
   ("r" nix-flake-init--from-registry)]
  ["Initialize a flake"
   ("t" "Select template" nix-flake-init-select-template)]
  (interactive (list nil))
  (when flake-ref
    (setq nix-flake-template-repository flake-ref))
  (transient-setup 'nix-flake-init-dispatch))

;;;###autoload
(defun nix-flake-init ()
  "Run \"nix flake init\" command via a transient interface."
  (interactive)
  (let* ((root (ignore-errors
		 (vc-root-dir)))
         (default-directory
           (if (and root
                    (not (file-equal-p root default-directory))
                    (yes-or-no-p (format-message
                                  "The directory %s is not the repository root. Change to %s?"
                                  default-directory root)))
               root
             default-directory)))
    (if (file-exists-p "flake.nix")
        (user-error "This directory already contains a flake")
      (nix-flake-init-dispatch))))

(provide 'nix-flake)
;;; nix-flake.el ends here
