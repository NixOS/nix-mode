;;; nix-shell.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; To use this just run:

;; M-x RET nix-shell RET

;; This will give you some

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-store)

(defgroup nix-shell nil
  "All nix-shell options."
  :group 'nix)

(defcustom nix-shell-file nil
  "Set to the file to run the nix-shell for."
  :type 'string
  :group 'nix-shell)

(defcustom nix-shell-attribute nil
  "Set to the file to run the nix-shell for."
  :type 'string
  :group 'nix-shell)

(defcustom nix-shell-inputs '(depsBuildBuild
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
					nix-shell-inputs)))))

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
	  (add-to-list 'woman-manpath man)
	  (add-to-list 'ffap-c-path include)
	  (add-to-list 'Man-header-file-path include)
	  (add-to-list 'irony-additional-clang-options
		       (format "-I%s" include))))

      (when flycheck-mode
	(flycheck-buffer))
      )))

;;;###autoload
(defun nix-shell-with-packages (packages &optional pkgs-file)
  "Create a nix shell environment from the listed package.
PACKAGES a list of packages to use.
PKGS-FILE the Nix file to get the packages from."
  (nix-instantiate-async (apply-partially 'nix-shell--callback
					  (current-buffer))
			 (nix-shell--with-packages-file packages pkgs-file)
			 ))

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
      (mapcar (lambda (x) (insert (format "	  %s\n" x))) packages)
      (insert "	 ];\n")
      (insert "} \"\"\n"))
    nix-file))

(defun nix-eshell-with-packages (packages &optional pkgs-file)
  "Create an Eshell buffer that has the shell environment in it.
PACKAGES a list of packages to pull in.
PKGS-FILE a file to use to get the packages."
  (let ((buffer (generate-new-buffer "*nix-eshell*")))
    (pop-to-buffer-same-window buffer)

    (setq-local nix-shell-clear-environment t)

    (nix-shell--callback
     (current-buffer)
     (nix-instantiate
      (nix-shell--with-packages-file packages pkgs-file)))

    (eshell-mode)
    buffer))

(defun nix-eshell (&optional nix-file attribute)
  "Create an Eshell buffer that has the shell environment in it.
NIX-FILE the .nix expression to create a shell for.
ATTRIBUTE attribute to instantiate in NIX-FILE."
  (interactive)

  (unless nix-file
    (when nix-shell-file (setq nix-file nix-shell-file)))

  (unless nix-file (error "Cannot find .nix file"))

  (let ((buffer (generate-new-buffer "*nix-eshell*")))
    (pop-to-buffer-same-window buffer)

    (setq-local nix-shell-clear-environment t)

    (nix-shell--callback
     (current-buffer)
     (nix-instantiate nix-file attribute))

    (eshell-mode)
    buffer))

;;;###autoload
(defun nix-shell (&optional nix-file attribute)
  "A nix-shell emulator in Emacs.
NIX-FILE the file to instantiate.
ATTRIBUTE an attribute of the Nix file to use."
  (interactive)

  (unless nix-file
    (when nix-shell-file (setq nix-file nix-shell-file)))

  (unless attribute
    (when nix-shell-attribute (setq attribute nix-shell-attribute)))

  (when nix-file
    (setq nix-file
	  (expand-file-name nix-file (locate-dominating-file
				      (if (buffer-file-name)
					  (buffer-file-name)
					default-directory)
				      dir-locals-file)))
    (nix-instantiate-async (apply-partially 'nix-shell--callback
					    (current-buffer))
			   nix-file attribute)))

(provide 'nix-shell)
;;; nix-shell.el ends here
