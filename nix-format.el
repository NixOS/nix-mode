;;; nix-format.el --- Nix formatter -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; Homepage: https://github.com/NixOS/nix-mode

;;; Commentary:

;;; Code:
(require 'reformatter)

(defcustom nix-nixfmt-bin "nixfmt"
  "Path to nixfmt executable."
  :group 'nix
  :type 'string)

;;;###autoload (autoload 'nixfmt-buffer "nix-format")
;;;###autoload (autoload 'nixfmt-region "nix-format")
;;;###autoload (autoload 'nixfmt-on-save-mode "nix-format")
(reformatter-define nixfmt
  :program nix-nixfmt-bin
  :args (list input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file-in-current-directory)
  :group 'nix)

(provide 'nix-format)
;;; nix-format.el ends here
