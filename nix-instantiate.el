;;; nix-instantiate.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'json)

(defun nix-instantiate--parsed (drv)
  "Get the parsed version of the .drv file.
DRV file to load from."
  (let ((stdout (generate-new-buffer "nix show-derivation"))
	result)
    (call-process nix-executable nil (list stdout nil) nil
		  "show-derivation" drv)
    (setq result
	  (cdar (with-current-buffer stdout
		  (when (eq (buffer-size) 0)
		    (error "Nix’s show-derivation %s failed to produce any output"
			   drv))
		  (goto-char (point-min))
		  (json-read))))
    (kill-buffer stdout)
    result))

(defun nix-instantiate (nix-file &optional attribute parse)
  "Run nix-instantiate on a Nix expression.
NIX-FILE the file to instantiate.
ATTRIBUTE an attribute of the Nix file to use.
PARSE whether to parse nix-instantiate output."
  (interactive (list (read-file-name "Nix file: ") nil t))

  (let ((stdout (generate-new-buffer "nix-instantiate"))
	result)
    (if attribute
	(call-process nix-instantiate-executable nil (list stdout nil) nil
		      nix-file "-A" attribute)
      (call-process nix-instantiate-executable nil (list stdout nil) nil
		    nix-file))
    (with-current-buffer stdout
      (when (eq (buffer-size) 0)
	(error
	 "Error: nix-instantiate %s failed to produce any output"
	 nix-file))
      (setq result (substring (buffer-string) 0 (- (buffer-size) 1)))
      (when parse
	(setq result (nix-instantiate--parsed result))))
    (kill-buffer stdout)
    result))

(defvar nix-instantiate--running-processes nil)

(defun nix-instantiate--sentinel (prop err proc event)
  "Make a nix-instantiate process.
PROP the prop name of nix-instantiate--running-processes.
ERR the error buffer.
PROC the process that has been run.
EVENT the event that was fired."
  (when (string= event "finished\n")
    (with-current-buffer (process-buffer proc)
      (unless (eq (buffer-size) 0)
	(let ((drv (nix-instantiate--parsed
		    (substring (buffer-string) 0 (- (buffer-size) 1)))))
	  (dolist
	      (callback (lax-plist-get nix-instantiate--running-processes prop))
	    (funcall callback drv)))))
    (setq nix-instantiate--running-processes
	  (lax-plist-put nix-instantiate--running-processes prop nil)))
  (unless (process-live-p proc)
    (kill-buffer (process-buffer proc))
    (kill-buffer err)))

(defun nix-instantiate-async (callback nix-file &optional attribute)
  "Run nix-instantiate on a Nix expression, asynchronously.
CALLBACK the function to call when instantiate completes.
NIX-FILE the file to instantiate
ATTRIBUTE an attribute of the Nix file to use."
  (setq nix-file (expand-file-name nix-file))
  (let* ((prop (if attribute
		   (expand-file-name attribute nix-file) nix-file))
	 (data (lax-plist-get nix-instantiate--running-processes prop))
	 (stdout (generate-new-buffer "nix-instantiate"))
	 (stderr (generate-new-buffer "nix-instantiate error")))
    (setq nix-instantiate--running-processes
	  (lax-plist-put nix-instantiate--running-processes
			 prop (cons callback data)))
    (make-process
     :name "nix-instantiate"
     :buffer stdout
     :command (append (list nix-instantiate-executable nix-file)
		      (when attribute (list "-A" attribute)))
     :noquery t
     :sentinel (apply-partially 'nix-instantiate--sentinel prop stderr)
     :stderr stderr)))

(provide 'nix-instantiate)
;;; nix-instantiate.el ends here
