;;; nix-repl.el --- Nix repl -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; Homepage: https://github.com/NixOS/nix-mode
;; Version: 1.4.5
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar nix-prompt-regexp "nix-repl> ")

(require 'comint)
(require 'nix)

(defgroup nix-repl nil
  "Nix-repl customizations."
  :group 'nix)

(defcustom nix-repl-executable-args '("repl")
  "Arguments to provide to nix-repl."
  :type '(repeat string))

(defvar nix-repl-completion-redirect-buffer
  " *nix-repl completions redirect*"
  "Buffer to be used to redirect output of readline commands.")

(defcustom nix-repl-completion-output-timeout 1.0
  "Time in seconds to wait for completion output before giving up."
  :type 'float)

(defvar nix-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'completion-at-point)
    map))

(defun nix-repl-save-all-histories ()
  "Call `comint-write-input-ring' for all `nix-repl-mode' buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'nix-repl-mode)
        (comint-write-input-ring)))))

(define-derived-mode nix-repl-mode comint-mode "Nix-REPL"
  "Interactive prompt for Nix."
  :interactive nil
  (setq-local comint-prompt-regexp nix-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (let* ((is-remote (file-remote-p default-directory))
         (maybe-xdg-data-home (if is-remote
                                  (shell-command-to-string "echo -n $XDG_DATA_HOME")
                                (or (getenv "XDG_DATA_HOME")
                                    "")))
         (path-prefix (if (string-empty-p maybe-xdg-data-home)
                          "~/.local/share"
                        maybe-xdg-data-home))
         (history-path (concat
                        is-remote
                        path-prefix
                        "/nix/repl-history")))
    (setq-local comint-input-ring-file-name history-path))
  (comint-read-input-ring t)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring nil 'local)
  (add-hook 'kill-emacs-hook #'nix-repl-save-all-histories nil 'local)
  (add-hook 'completion-at-point-functions
            #'nix-repl-completion-at-point nil 'local))

(defmacro nix--with-temp-process-filter (proc &rest body)
  "Use temp process PROC filter on BODY."
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

;;;###autoload
(defun nix-repl ()
  "Load the Nix-REPL."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create "*Nix-REPL*"))
  (unless (comint-check-proc (current-buffer))
    (nix--make-repl-in-buffer (current-buffer))
    (nix-repl-mode)))

(defalias 'nix-repl-show 'nix-repl)

(defun nix--make-repl-in-buffer (buffer)
  "Make Nix Repl in BUFFER."
  (apply
   'make-comint-in-buffer
   (append `("Nix-REPL" ,buffer ,nix-executable nil)
           nix-repl-executable-args)))

(defun nix-get-completions (process input)
  "Get completions for INPUT using native readline for PROCESS."
  (with-current-buffer (process-buffer process)
    (let* ((original-filter-fn (process-filter process))
           (redirect-buffer (get-buffer-create
                             nix-repl-completion-redirect-buffer))
           (trigger "\t")
           (new-input (concat input trigger))
           (input-length
            (save-excursion
              (+ (- (point-max) (comint-bol)) (length new-input))))
           (delete-line-command (make-string input-length ?\b))
           (input-to-send (concat new-input delete-line-command)))
      ;; Ensure restoring the process filter, even if the user quits
      ;; or there's some other error.
      (unwind-protect
          (with-current-buffer redirect-buffer
            ;; Cleanup the redirect buffer
            (erase-buffer)
            ;; Mimic `comint-redirect-send-command', unfortunately it
            ;; can't be used here because it expects a newline in the
            ;; command and that's exactly what we are trying to avoid.
            (let ((comint-redirect-echo-input nil)
                  (comint-redirect-completed nil)
                  (comint-redirect-perform-sanity-check nil)
                  (comint-redirect-insert-matching-regexp t)
                  (comint-redirect-finished-regexp nix-prompt-regexp)
                  (comint-redirect-output-buffer redirect-buffer))
              (set-process-filter
               process (apply-partially
                        #'comint-redirect-filter original-filter-fn))
              (process-send-string process input-to-send)
              ;; Grab output until our dummy completion used as
              ;; output end marker is found.
              (when (nix--accept-process-output
                     process nix-repl-completion-output-timeout
                     comint-redirect-finished-regexp)
                (beginning-of-line)
                (if (eq (char-after) ?\r)
                    (cdr
                     (split-string
                      (buffer-substring-no-properties
                       (line-beginning-position) (point-min))
                      "[ \f\t\n\r\v]+" t))
                  (search-forward "" nil t)
                  (backward-char)
                  (if (eq (char-before) ?\a)
                      nil
                    (list (buffer-substring-no-properties (line-beginning-position) (point))))))))
        (set-process-filter process original-filter-fn)))))

(defun nix--accept-process-output (process &optional timeout regexp)
  "Accept PROCESS output with TIMEOUT until REGEXP is found.
Optional argument TIMEOUT is the timeout argument to
`accept-process-output' calls.  Optional argument REGEXP
overrides the regexp to match the end of output, defaults to
`comint-prompt-regexp'.  Returns non-nil when output was
properly captured.

This utility is useful in situations where the output may be
received in chunks, since `accept-process-output' gives no
guarantees they will be grabbed in a single call."
  (let ((regexp (or regexp comint-prompt-regexp)))
    (catch 'found
      (while t
        (when (not (accept-process-output process timeout))
          (throw 'found nil))
        (when (progn (re-search-backward regexp nil t))
          (throw 'found t))))))

;;;###autoload
(defun nix-repl-completion-at-point ()
  "Completion at point function for Nix using \"nix-repl\".
See `completion-at-point-functions'."
  (save-excursion
    (let* ((proc (get-buffer-process (current-buffer)))
           (prefix (and (derived-mode-p 'nix-repl-mode)
                        proc
                        (executable-find nix-executable)
                        (nix--prefix-bounds))))
      (pcase prefix
        (`(,beg . ,end)
         (list beg end
               (nix-get-completions
                proc
                (buffer-substring beg end))
               :exclusive 'no))))))

(defun nix--prefix-bounds ()
  "Get bounds of Nix attribute path at point as a (BEG . END) pair, or nil."
  (save-excursion
    (when (< (skip-chars-backward "a-zA-Z0-9'\\-_\\.") 0)
      (cons (point) (+ (point) (skip-chars-forward "a-zA-Z0-9'\\-_\\."))))))

(defun nix--send-repl (input &optional process mute)
  "Send INPUT to PROCESS.

MUTE if true then don’t alert user."
  (let ((proc (or process (get-buffer-process (current-buffer)))))
    (if mute
        (nix--with-temp-process-filter proc
          (process-send-string proc input))
      (process-send-string proc input))))

(defun nix--char-with-ctrl (char)
  "Generate control character CHAR."
  (char-to-string (logand #b10011111 char)))

(defun nix--process-filter (buf marker)
  "Process filter for Nix-rel buffer BUF at MARKER."
  (lambda (_proc string)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char marker)
          (insert string)
          (set-marker marker (point)))))))

(provide 'nix-repl)
;;; nix-repl.el ends here
