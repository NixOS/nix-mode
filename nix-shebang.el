;;; nix-shebang.el --- Handle nix shebang header -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Version: 1.2.1
;; Keywords: nix, languages, tools, unix
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This detects file headers that look like:
;; #!/usr/bin/env nix-shell
;; #!nix-shell -i bash

;; and correctly detects their file modes.

;;; Code:

(require 'files)

(defvar nix-shebang-interpreter-regexp "#!\s*nix-shell -i \\([^ \t\n]+\\)"
  "Regexp for nix-shell -i header.")

(defun nix-shebang-get-interpreter ()
  "Get interpreter string from nix-shell -i file."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (when (looking-at nix-shebang-interpreter-regexp)
      (match-string 1))))

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
