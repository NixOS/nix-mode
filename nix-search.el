;;; nix-search.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix
;; Version: 1.4.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-shell)
(require 'json)

;;;###autoload
(defun nix-search--search (search file &optional no-cache)
  (with-temp-buffer
    (call-process nix-executable nil (list t nil) nil
      "search" "--json" (if no-cache "--no-cache" "") "--file" file search)
    (goto-char (point-min))
    (json-read)))

;;;###autoload
(defun nix-search--display (results &optional display-buffer)
  (unless display-buffer (setq display-buffer (generate-new-buffer "*nix search*")))
  (with-current-buffer display-buffer
    (dolist (entry results)
      (widget-insert
        (format "attr: %s\nname: %s\nversion: %s\ndescription: %s\n\n"
          (car entry)
          (alist-get 'pkgName (cdr entry))
          (alist-get 'version (cdr entry))
          (alist-get 'description (cdr entry))))))
  (display-buffer display-buffer))

;;;###autoload
(defun nix-search (search &optional file)
  "Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in."
  (interactive "snix-search> \n")
  (setq file (or file (nix-read-file)))
  (let ((results (nix-search--search search file)))
    (when (called-interactively-p 'any)
      (nix-search--display results))
    results))

(defun nix-search-read-attr (file)
  "Read from a list of attributes.
FILE the nix file to look in."
  (let ((collection
	 (sort (mapcar (lambda (x) (symbol-name (car x)))
		       (nix-search "" file))
	       'string<))
	(read (cond ((fboundp 'ivy-read) 'ivy-read)
		    (t 'completing-read))))
    (funcall read "Attribute: " collection)))

(provide 'nix-search)
;;; nix-search.el ends here
