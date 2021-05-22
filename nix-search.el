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
(defun nix-search--search (search file &optional no-cache use-flakes)
  (with-temp-buffer
    (if use-flakes
	(call-process nix-executable nil (list t nil) nil
		      "search" "--json" file (if (string= search "") "." search))
      (call-process nix-executable nil (list t nil) nil
		    "search" "--json" (if no-cache "--no-cache" "") "--file" file search))
    (goto-char (point-min))
    (json-read)))

;;;###autoload
(defun nix-search--display (results &optional display-buffer use-flakes)
  (unless display-buffer (setq display-buffer (generate-new-buffer "*nix search*")))
  (with-current-buffer display-buffer
    (dolist (entry results)
      (widget-insert
       (format "attr: %s\nname: %s\nversion: %s\ndescription: %s\n\n"
               (car entry)
               (if use-flakes
		   (alist-get 'pname (cdr entry))
		 (alist-get 'pkgName (cdr entry)))
               (alist-get 'version (cdr entry))
               (alist-get 'description (cdr entry))))))
  (display-buffer display-buffer))

;;;###autoload
(defun nix-search (search &optional file)
  "Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in."
  (interactive "snix-search> \n")
  (setq use-flakes (nix-has-flakes))
  (setq file (or file (if use-flakes (nix-read-flake) (nix-read-file))))
  (let ((results (nix-search--search search file nil use-flakes)))
    (when (called-interactively-p 'any)
      (nix-search--display results nil use-flakes))
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
