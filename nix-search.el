;;; nix-search.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-shell)
(require 'json)

;;;###autoload
(defun nix-search (&optional search file)
  "Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in."
  (interactive)
  (unless search (setq search ""))
  (unless file (nix-read-file))

  (let ((stdout (generate-new-buffer "nix search"))
	result)
    (call-process nix-executable nil (list stdout nil) nil
		  "search" "--json" "-f" file search)
    (with-current-buffer stdout
      (when (eq (buffer-size) 0)
	(error "Error: nix search %s failed to produce any output" search))
      (goto-char (point-min))
      (setq result (json-read)))
    (kill-buffer stdout)
    (when (called-interactively-p 'any)
      (let ((display (generate-new-buffer "*nix search*")))
	(with-current-buffer display
	  (dolist (entry result)
	    (widget-insert
	     (format "attr: %s\nname: %s\nversion: %s\ndescription: %s\n\n"
                     (car entry)
		     (alist-get 'pkgName (cdr entry))
		     (alist-get 'version (cdr entry))
		     (alist-get 'description (cdr entry)))))
	  )
	(display-buffer display 'display-buffer-pop-up-window)))
    (kill-buffer stdout)
    result))

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
