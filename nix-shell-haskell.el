;;; nix-shell-haskell.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; You can use nix-shell-haskell as a hook to haskell-mode.
;; Just run:

;; (require 'nix-shell-haskell)
;; (add-hook 'haskell-mode 'nix-shell-haskell)

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-shell)
(require 'haskell)
(require 'haskell-cabal)
(require 'haskell-session)
(require 'flycheck)

(defun nix-shell-haskell--callback (buffer cabal-file drv &rest _)
  "Run the nix-shell callback to setup the buffer.
The BUFFER to run in.
The CABAL-FILE to load from.
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
      (make-local-variable 'exec-path)

      (dolist (input inputs)
	(when (and (not (file-directory-p input))
		   nix-shell-auto-realise)
	  (nix-store-realise input))

	(add-to-list 'exec-path (expand-file-name "bin" input)))

      (when flycheck-mode
	(flycheck-buffer)))

    (when (and (eq major-mode 'haskell-mode) (not (haskell-session-maybe)))
      (message "here")
      (unless (haskell-session-lookup cabal-file)
	(let ((session (haskell-session-make cabal-file)))
	  (haskell-session-set-cabal-dir (file-name-directory cabal-file))
	  (haskell-process-start session)))
      (haskell-session-assign cabal-file))))

;;;###autoload
(defun nix-shell-haskell (&optional cabal-file)
  "Setup Nix shell from the .cabal file."
  (interactive)
  (unless cabal-file
    (setq cabal-file (haskell-cabal-find-file)))
  (when (and (called-interactively-p) (not cabal-file))
    (error "Cannot find a cabal file in %s" default-directory))
  (when cabal-file
    (setq cabal-file (expand-file-name cabal-file))
    (let ((file (concat (file-name-sans-extension cabal-file) ".nix"))
	  (project-dir (locate-dominating-file cabal-file "default.nix")))
      (with-temp-file file
	(insert "let\n")
	(insert (format "  haskellPackages = %s.packages"
			(if project-dir (format "(import %s {})"
						(expand-file-name
						 "default.nix" project-dir))
			  "{}")))
	(insert " or (import <nixpkgs> {}).haskellPackages;\n")
	(insert "in\n")
	(insert (format "  (haskellPackages.callCabal2nix \"%s\" \"%s\" {}).env"
			(file-name-base cabal-file) cabal-file))
	(insert "  .overrideAttrs (attrs: {\n")
	(insert "    nativeBuildInputs = (attrs.nativeBuildInputs or [])\n")
	(insert "                        ++ (with haskellPackages; [ cabal-install ]);\n")
	(insert "  })\n"))
      (nix-instantiate-async (apply-partially 'nix-shell-haskell--callback
					      (current-buffer)
					      cabal-file)
			     file))))

(provide 'nix-shell-haskell)
;;; nix-shell-haskell.el ends here
