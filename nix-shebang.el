;;; nix-shebang.el --- Handle nix shebang header -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, languages, tools, unix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This detects file headers that look like:
;; #!/usr/bin/env nix-shell
;; #!nix-shell -i bash
;; As well as:
;; #!/usr/bin/env nix
;; #! nix shell nixpkgs#bash nixpkgs#hello nixpkgs#cowsay --command bash

;; and correctly detects their file modes.

;;; Code:

(require 'files)

(defvar nix-shebang-interpreter-regexp "#!\s*nix-shell -i \\([^ \t\n]+\\)"
  "Regexp for nix-shell -i header.")
(defvar nix-shebang-flake-interpreter-regexp "^#!\s*nix .*--command \\(.*\\)"
  "Regexp for nix shell script using flakes.")

(defun nix-shebang-get-interpreter ()
  "Get interpreter string"
  (save-excursion
    (goto-char (point-min))
	(forward-line 1)
    (if (looking-at nix-shebang-interpreter-regexp)
		(match-string 1)
	  (if (and (re-search-forward nix-shebang-flake-interpreter-regexp nil t)
			   (looking-back nix-shebang-flake-interpreter-regexp))
          (match-string 1)))))

(defun nix-shebang-mode ()
  "Detect and run file’s interpreter mode."
  (let ((mode (nix-shebang-get-interpreter)))
    (when mode
      (funcall (assoc-default mode
                              (mapcar (lambda (e)
                                        (cons
                                         (format "\\`%s\\'" (car e))
                                         (cdr e)))
                                      interpreter-mode-alist)
                              #'string-match-p)))))

(provide 'nix-shebang)
;;; nix-shebang.el ends here
