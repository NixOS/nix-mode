;;; nix-repl.el --- Nix repl
;; -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar nix-prompt-regexp "nix-repl> ")

(define-derived-mode nix-repl-mode comint-mode "Nix-REPL"
  "Interactive prompt for Nix."
  (setq-local comint-prompt-regexp nix-prompt-regexp)
  (setq-local comint-prompt-read-only t))

(defun nix-repl-show ()
  "Load the Nix-REPL."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create "*Nix-REPL*"))
  (unless (comint-check-proc (current-buffer))
    (nix--make-repl-in-buffer (current-buffer))
    (nix-repl-mode)))

(defun nix--make-repl-in-buffer (buffer)
  (make-comint-in-buffer "Nix-REPL" buffer "nix-repl"))

(defun nix-get-completions (proc prefix)
  (save-match-data
    (nix--with-temp-process-filter proc
      (goto-char (point-min))
      (process-send-string proc (concat prefix "\t\"" (nix--char-with-ctrl ?a) "\"\n"))
      (setq i 0)
      (while (and (< (setq i (1+ i)) 100)
                  (not (search-forward-regexp "\"\\([^\"]*\\)\"[\n]*nix-repl>" nil t)))
        (sleep-for 0.01))
      (let ((new-prefix (match-string 1))
            (start-compl (point)))
        (if (string-suffix-p " " new-prefix)
            (list (substring new-prefix 0 -1))
          (process-send-string proc (concat new-prefix "\t\t" (nix--char-with-ctrl ?u) "\n"))
          (goto-char start-compl)
          (setq i 0)
          (while (and (< (setq i (1+ i)) 100)
                      (not (search-forward-regexp
                            "[\n]+nix-repl>\\|Display all \\([0-9]+\\)" nil t)))
            (sleep-for 0.01))
          (if (match-string 1)
              (progn
                (process-send-string proc "n")
                '())
            (search-backward "\n" nil t)
            (split-string (buffer-substring start-compl (1- (point))))))))))

(defun nix--send-repl (input &optional process mute)
  (let ((proc (or process (get-buffer-process (current-buffer)))))
    (if mute
        (nix--with-temp-process-filter proc
          (process-send-string proc input))
      (process-send-string proc input))))

(defun nix--char-with-ctrl (char)
  (char-to-string (logand #b10011111 char)))

(defmacro nix--with-temp-process-filter (proc &rest body)
  (declare (indent defun))
  `(let* ((buf (generate-new-buffer " *temp-process-output*"))
          (proc-filter-saved (process-filter ,proc))
          (proc-marker (with-current-buffer buf (point-marker))))
     (set-process-filter ,proc (nix--process-filter buf proc-marker))
     (unwind-protect
         (with-current-buffer buf
           ,@body)
       (set-process-filter ,proc proc-filter-saved)
       (kill-buffer buf))))

(defun nix--process-filter (buf marker)
  (lambda (proc string)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char marker)
          (insert string)
          (set-marker marker (point)))))))

(provide 'nix-repl)
;;; nix-repl.el ends here
