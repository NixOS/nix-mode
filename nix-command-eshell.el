;;; nix-command-eshell.el --- eshell support for nix-command -*- lexical-binding: t -*-

;; Author: Patryk Wychowaniec <pwychowaniec@pm.me>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, tools, eshell

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Provides an eshell-aware `nix' command[1].
;;
;; Using this package causes `nix develop' and `nix shell' to extend the current
;; eshell session instead of spawning an external shell; other Nix commands get
;; passed-through.
;;
;; This package also advices `eshell/exit' so that if a Nix shell is currently
;; active, it gets deactivated first and only the next call to `exit' actually
;; closes eshell. Note that exiting restores the previous working directory as
;; well - you can customize that with `nix-command-eshell-exit-behavior'.
;;
;; Nested shells are supported, so for instance:
;;
;;    $ nix shell 'nixpkgs#netris'
;;    $ nix shell 'nixpkgs#gcc'
;;
;; ... will bring both `netris' and `gcc' into scope - you'll have to invoke
;; `exit' twice to go back to your original shell then.
;;
;; When enabling this package, it is recommended that you extend your prompt to
;; check `nix-command-eshell-active-p' so that you know whether you're inside a
;; Nix shell or not, like:
;;
;;     (defun +eshell/prompt ()
;;       (concat
;;        (eshell/pwd)
;;        (if (nix-command-eshell-active-p) " | nix" "")
;;        " "))
;;
;; This package is compatible with `eat' and `envrc'.
;;
;; Finally, if any of the functionalities provided here malfunctions, remember
;; that you can always use `*' to avoid going through `eshell/nix' - that is,
;; `*nix develop' will start an external shell.
;;
;; [1] https://nixos.wiki/wiki/Nix_command

;;; Code:

(require 'eshell)
(require 'esh-mode)

;; ---

(defgroup nix-command-eshell nil
  "Nix-Eshell integration."
  :group 'nix)

(defcustom nix-command-eshell-exit-behavior 'restore-pwd
  "Behavior of the `exit' command when inside a Nix shell.

This option determines whether `exit' should keep the current directory or
rather whether it should restore the directory where `nix' was called. The later
simulates an actual subshell better, but can be also annoying for some people."
  :type '(choice
          (const :tag "Keep current directory on exit" keep-pwd)
          (const :tag "Restore previous directory on exit" restore-pwd))
  :group 'nix-command-eshell)

;; ---

(defvar nix-command-eshell--shells '())

(push "nix" eshell-complex-commands)

;;;###autoload
(defun eshell/nix (&rest args)
  "eshell-aware wrapper for the `nix' command."
  (let ((cmd (car args)))
    (if (and (or (string= "develop" cmd) (string= "shell" cmd))
             (not (member "-c" args)))
        (nix-command-eshell--spawn args)
      (throw 'eshell-replace-command
             (eshell-parse-command
              (concat (char-to-string eshell-explicit-command-char) "nix") args)))))

;;;###autoload
(defun eshell/nix-leave ()
  "Leave Nix subshell without returning to the previous directory."
  (when (nix-command-eshell-active-p)
    (let ((curr-pwd default-directory))
      (nix-command-eshell--leave)
      (setq default-directory curr-pwd)
      ())))

(defun nix-command-eshell-active-p ()
  "Return non-nil when we're inside a Nix shell."
  (not (eq nix-command-eshell--shells '())))

(defun nix-command-eshell--spawn (args)
  (let* ((temp-file
          (make-temp-file "nix"))
         (command
          (append '("nix") args (list "--command" "sh" "-c" (format "export > %s" temp-file))))
         (proc
          (make-process
           :name "nix"
           :buffer (current-buffer)
           :command command
           :filter 'nix-command-eshell--filter
           :sentinel 'nix-command-eshell--sentinel)))
    (process-put proc 'temp-file temp-file)
    (eshell-record-process-object proc)
    (eshell-record-process-properties proc)
    (throw 'eshell-external proc)))

(defun nix-command-eshell--filter (proc output)
  (if (fboundp 'eshell-interactive-process-filter)
      ;; For Emacs >= 30
      (eshell-interactive-process-filter proc output))
  ;; For Emacs < 30
  (eshell-insertion-filter proc output))

(defun nix-command-eshell--sentinel (proc status)
  (let ((cmd
         (if (fboundp 'eshell-commands-for-process)
             ;; For Emacs >= 30
             (car (eshell-commands-for-process proc))
           ;; For Emacs < 30
           t))
        (buffer (process-buffer proc))
        (temp-file (process-get proc 'temp-file)))
    (when (and cmd
               (buffer-live-p buffer)
               (eq 0 (process-exit-status proc)))
      (with-current-buffer buffer
        (nix-command-eshell--enter
         (nix-command-eshell--parse temp-file))))
    (when (not (process-live-p proc))
      (delete-file temp-file)))
  (eshell-sentinel proc status))

(defun nix-command-eshell--parse (path)
  (let ((env '())
        (regex
         (rx "export "
             (group (one-or-more (or alpha ?_)))
             (opt
              "=\""
              (group (zero-or-more (not "\"")))))))
    (with-temp-buffer
      (insert-file-contents path)
      (while (search-forward-regexp regex nil t 1)
        (let ((key (match-string 1))
              (val (match-string 2)))
          (if val
              (push (format "%s=%s" key val) env)
            (push key env)))))
    env))

(defun nix-command-eshell--enter (env)
  (make-local-variable 'nix-command-eshell--shells)
  (push (list :dir default-directory
              :env env)
        nix-command-eshell--shells)
  (nix-command-eshell--refresh))

(defun nix-command-eshell--leave ()
  (when-let* ((shell (pop nix-command-eshell--shells)))
    (when (eq 'restore-pwd nix-command-eshell-exit-behavior)
      (let ((dir (plist-get shell :dir)))
        (when (file-directory-p dir)
          (setq-local default-directory dir))))
    (nix-command-eshell--refresh))
  (when (not (nix-command-eshell-active-p))
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)
    (when (fboundp 'envrc--update)
      (envrc--update))))

(defun nix-command-eshell--refresh ()
  (nix-command-eshell--refresh-env)
  (nix-command-eshell--refresh-path))

(defun nix-command-eshell--refresh-env ()
  (setq-local process-environment '())
  (dolist (shell nix-command-eshell--shells)
    (setq-local process-environment
                (append process-environment (plist-get shell :env))))
  (dolist (val (or (default-value 'process-environment) '()))
    (push process-environment val)))

(defun nix-command-eshell--refresh-path ()
  (setq-local exec-path
              (parse-colon-path
               (getenv-internal "PATH" process-environment)))
  (dolist (val (or (default-value 'exec-path) '()))
    (push exec-path val))
  (eshell-set-path (getenv "PATH")))

;; ---

(defun nix-command-eshell--exit-a (fn &rest args)
  (if (nix-command-eshell-active-p)
      (progn (nix-command-eshell--leave) ())
    (apply fn args)))

(advice-add 'eshell/exit :around 'nix-command-eshell--exit-a)

;; ---

(defun nix-command-eshell--envrc-clear-a (&rest _)
  (when (nix-command-eshell-active-p)
    (nix-command-eshell--refresh)))

(advice-add 'envrc--clear :after 'nix-command-eshell--envrc-clear-a)

;; ---

(provide 'nix-command-eshell)
