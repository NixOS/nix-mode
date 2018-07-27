;;; nix.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; To use this just run:

;; M-x RET nix-shell RET

;; This will give you some

;;; Code:

(require 'nix-mode)
(require 'nix-shebang)
(require 'nix-shell)
(require 'nix-repl)
(require 'nix-drv-mode)

(defgroup nix nil
  "Nix-related customizations"
  :group 'languages)

(defcustom nix-executable (executable-find "nix")
  "Nix executable location."
  :group 'nix
  :type 'string)

(defcustom nix-build-executable (executable-find "nix-build")
  "Nix-build executable location."
  :group 'nix
  :type 'string)

(defcustom nix-instantiate-executable (executable-find "nix-instantiate")
  "Nix executable location."
  :group 'nix
  :type 'string)

(defcustom nix-store-executable (executable-find "nix-store")
  "Nix executable location."
  :group 'nix
  :type 'string)

(defcustom nix-shell-executable (executable-find "nix-shell")
  "Location of ‘nix-shell’ executable."
  :group 'nix
  :type 'string)

(defun nix-system ()
  "Get the current system tuple."
  (let ((stdout (generate-new-buffer "nix eval"))
        result)
    (call-process nix-executable nil (list stdout nil) nil
		  "eval" "--raw" "(builtins.currentSystem)")
    (with-current-buffer stdout (setq result (buffer-string)))
    (kill-buffer stdout)
    result))

;;;###autoload
(defun nix-build (&optional attr dir)
  "Run nix-build.
ATTR is the attribute to build.
DIR is the directory containing the Nix default.nix expression."
  (interactive)
  (unless dir (setq dir default-directory))
  (if attr
      (async-shell-command (format "%s %s -A %s" nix-build-executable dir attr))
    (async-shell-command (format "%s %s" nix-build-executable dir))))

;;;###autoload
(defun nix-unpack (path attribute)
  "Get source from a Nix derivation.

PATH used for base of Nix expresions.

ATTRIBUTE from PATH to get Nix expressions from."
  (interactive (list (read-string "Nix path: " "<nixpkgs>")
		     (read-string "Nix attribute name: ")))
  (async-shell-command (format "%s '%s' -A '%s' --run unpackPhase"
			       nix-shell-executable
			       path attribute)))

;;;###autoload
(define-minor-mode global-nix-mode
  "Minor mode to enable Nix enhancements."
  :require 'nix
  :global t
  (when global-nix-mode
    (add-to-list 'interpreter-mode-alist '("nix-shell" . nix-shebang-mode))
    (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
    (add-to-list 'auto-mode-alist '("\\.drv\\'" . nix-drv-mode))
    (add-hook 'after-change-major-mode-hook 'nix-shell-mode)))

(provide 'nix)
;;; nix.el ends here
