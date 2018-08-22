;;; nix-store.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)

(defun nix-store-realise (path)
  "Realise a path asynchronously.
PATH the path within /nix/store to realise"
  (make-process
   :buffer nil
   :command (list nix-store-executable "-r" path)
   :noquery t
   :name (format "*nix-store*<%s>" path)))

(provide 'nix-store)
;;; nix-store.el ends here
