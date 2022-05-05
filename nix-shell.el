;;; nix-shell.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, processes
;; Version: 1.4.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; To use this just run:

;; M-x RET nix-shell RET

;; This will give you some

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-store)

;; Tell the byte compiler these are dynamically bound
(defvar woman-manpath)
(defvar Man-header-file-path)
(defvar irony-additional-clang-options)
(defvar eshell-path-env)
(defvar ffap-c-path)

(defgroup nix-shell nil
  "All nix-shell options."
  :group 'nix)

(defcustom nix-shell-inputs '(buildInputs
			      depsBuildBuild
			      depsBuildBuildPropagated
			      nativeBuildInputs
			      propagatedNativeBuildInputs
			      depsBuildTarget
			      depsBuildTargetPropagated)
  "List of inputs to collect for nix-shell."
  :type 'list
  :group 'nix-shell)

(defcustom nix-shell-clear-environment nil
  "Whether to clear the old ‘exec-path’ & environment.
Similar to ‘--pure’ argument in command line nix-shell."
  :type 'boolean
  :group 'nix-shell)

(defcustom nix-shell-auto-realise t
  "Whether we can realise paths in the built .drv file."
  :type 'boolean
  :group 'nix-shell)

(defcustom nix-file nil
  "Nix file to build expressions from.
Should only be set in dir-locals.el file."
  :type 'stringp
  :group 'nix-shell)

(defcustom nix-flake nil
  "Nix flake to build expressions from.
Should only be set in dir-locals.el file."
  :type 'stringp
  :group 'nix-shell)

(defcustom nix-attr nil
  "Nix attribute path to use.
Should only be set in dir-locals.el file."
  :type 'stringp
  :group 'nix-shell)

;;;###autoload
(defun nix-shell-unpack (file attr)
  "Run Nix’s unpackPhase.
FILE is the file to unpack from.
ATTR is the attribute to unpack."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (nix-shell--run-phase "unpack" file attr))

(defun nix-read-attr (_)
  "Get nix attribute from user."
  (read-string "Nix attr: "))

(defun nix-read-flake ()
  "Get nix flake from user."
  (cond
   (nix-flake nix-flake)
   ((and (nix-has-flakes) (file-exists-p "flake.nix")) ".")
   (t (read-string "Nix flake: " "nixpkgs"))))

(defun nix-read-file ()
  "Get nix file from user."
  (cond
   (nix-file nix-file)
   ((file-exists-p "shell.nix") "shell.nix")
   ((file-exists-p "default.nix") "default.nix")
   (t (read-file-name "Nix file: " nil "<nixpkgs>"))))

;;;###autoload
(defun nix-shell-configure (file attr)
  "Run Nix’s configurePhase.
FILE is the file to configure from.
ATTR is the attribute to configure."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (nix-shell--run-phase "configure" file attr))

;;;###autoload
(defun nix-shell-build (file attr)
  "Run Nix’s buildPhase.
FILE is the file to build from.
ATTR is the attribute to build."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (nix-shell--run-phase "build" file attr))

(defun nix-shell--run-phase (phase file attr)
  "Get source from a Nix derivation.
PHASE phase to run.
FILE used for base of Nix expresions.
ATTR from NIX-FILE to get Nix expressions from."
  (shell-command
   (format "%s '%s' -A '%s' --run 'if [ -z \"$%sPhase\" ]; then eval %sPhase; else eval \"$%sPhase\"; fi' &"
	   nix-shell-executable
	   file attr phase phase phase)))

(declare-function flycheck-buffer "flycheck")

(defun nix-shell--callback (buffer drv)
  "Run the nix-shell callback to setup the buffer.
The BUFFER to run in.
The DRV file to use."
  (let* ((env (alist-get 'env drv))
	 (stdenv (alist-get 'stdenv env))
	 (system (alist-get 'system env))
	 (inputs (remove nil
			 (apply 'append
				(mapcar (lambda (prop)
					  (split-string (alist-get prop env)))
					nix-shell-inputs))))
	 ;; This attribute is in `mkShell' — ideally, we'd only check this variable in those cases.
	 (ld-library-path (alist-get 'LD_LIBRARY_PATH env)))

    ;; Prevent accidentally rebuilding the world.
    (unless (file-directory-p stdenv)
      (error
       "Your stdenv at %s has not been built. Please run: nix-store -r %s"
       stdenv stdenv))

    ;; Make sure this .drv file can actually be built here.
    (unless (string= system (nix-system))
      (error
       "Your system (%s) does not match .drv’s build system (%s)"
       (nix-system) system))

    (with-current-buffer buffer
      (when nix-shell-clear-environment
	(setq-local exec-path nil)
	(setq-local eshell-path-env "")
	;; (setq-local process-environment nil)
	)

      ;; Set the LD_LIBRARY_PATH where applicable
      (when ld-library-path
	(make-local-variable 'process-environment)
	(setq process-environment
	      (cons
	       (let*
		   ((var "LD_LIBRARY_PATH")
		    (current-path (getenv var)))
		 (if current-path
		     ;; LD_LIBRARY_PATH defined in derivation takes precedence
		     (format "%s=%s:%s" var ld-library-path current-path)
		   (format "%s=%s" var ld-library-path)))
		    process-environment)))

      (dolist (input inputs)
	(when (and (not (file-directory-p input))
		   nix-shell-auto-realise)
	  (nix-store-realise input))

	(let ((bin (expand-file-name "bin" input))
	      (man (expand-file-name "share/man" input))
	      (include (expand-file-name "include" input)))
	  (add-to-list 'exec-path bin)
	  (setq-local eshell-path-env
		      (format "%s:%s" bin eshell-path-env))
      (when (boundp 'woman-manpath)
	    (add-to-list 'woman-manpath man))
	  (add-to-list 'ffap-c-path include)
	  (add-to-list 'Man-header-file-path include)
      (when (boundp 'irony-additional-clang-options)
	    (add-to-list 'irony-additional-clang-options
		       (format "-I%s" include)))))

      (when (bound-and-true-p flycheck-mode)
	(flycheck-buffer)))))

(defun nix-shell-with-packages (packages &optional pkgs-file)
  "Create a nix shell environment from the listed package.
PACKAGES a list of packages to use.
PKGS-FILE the Nix file to get the packages from."
  (nix-instantiate-async (apply-partially 'nix-shell--callback
					  (current-buffer))
			 (nix-shell--with-packages-file packages pkgs-file)))

(defun nix-shell--with-packages-file (packages &optional pkgs-file)
  "Get a .nix file from the packages list.
PACKAGES to put in the .nix file.
PKGS-FILE package set to pull from."
  (unless pkgs-file (setq pkgs-file "<nixpkgs>"))
  (let ((nix-file (make-temp-file "nix-shell" nil ".nix")))
    (with-temp-file nix-file
      (insert (format "with import %s { };\n" pkgs-file))
      (insert "runCommandCC \"shell\" {\n")
      (insert "	 nativeBuildInputs = [\n")
      (mapc (lambda (x) (insert (format "	  %s\n" x))) packages)
      (insert "	 ];\n")
      (insert "} \"\"\n"))
    nix-file))

;;;###autoload
(defun nix-eshell-with-packages (packages &optional pkgs-file)
  "Create an Eshell buffer that has the shell environment in it.
PACKAGES a list of packages to pull in.
PKGS-FILE a file to use to get the packages."
  (let ((buffer (generate-new-buffer "*nix-eshell*")))
    (pop-to-buffer-same-window buffer)

    (setq-local nix-shell-clear-environment t)

    ;; We must start this before the callback otherwise the path is cleared
    (eshell-mode)

    (nix-shell--callback
     (current-buffer)
     (nix-instantiate
      (nix-shell--with-packages-file packages pkgs-file) nil t))

    buffer))

;;;###autoload
(defun nix-eshell (file &optional attr)
  "Create an Eshell buffer that has the shell environment in it.
FILE the .nix expression to create a shell for.
ATTR attribute to instantiate in NIX-FILE."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr nix-file)))

  (let ((buffer (generate-new-buffer "*nix-eshell*")))
    (pop-to-buffer-same-window buffer)

    (setq-local nix-shell-clear-environment t)

    ;; We must start this before the callback otherwise the path is cleared
    (eshell-mode)

    (nix-shell--callback
     (current-buffer)
     (nix-instantiate file attr t))

    buffer))

;;;###autoload
(defun nix-shell-with-string (string)
  "A nix-shell emulator in Emacs from a string.
STRING the nix expression to use."
  (let ((file (make-temp-file "nix-shell" nil ".nix")))
    (with-temp-file file (insert string))
    (nix-instantiate-async (apply-partially 'nix-shell--callback
					    (current-buffer))
			   file)))

;;;###autoload
(defun nix-shell (file &optional attr)
  "A nix-shell emulator in Emacs.
FILE the file to instantiate.
ATTR an attribute of the Nix file to use."
  (interactive (list (nix-read-file) nil))
  (unless attr (setq attr (nix-read-attr file)))

  (nix-instantiate-async (apply-partially 'nix-shell--callback
					  (current-buffer))
			 file attr))

(provide 'nix-shell)
;;; nix-shell.el ends here
