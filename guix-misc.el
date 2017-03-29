;;; guix-misc.el --- Miscellaneous definitions  -*- lexical-binding: t -*-

;; Copyright © 2014–2017 Alex Kost <alezost@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some miscellaneous code that does not find its
;; home in any other file (in a perfect world this file wouldn't exist).

;;; Code:

(require 'cl-lib)
(require 'guix nil t)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-read)
(require 'guix-utils)
(require 'guix-ui)
(require 'guix-profiles)


;;; Actions on packages and generations

(defface guix-operation-option-key
  '((t :inherit font-lock-warning-face))
  "Face used for the keys of operation options."
  :group 'guix-faces)

(defcustom guix-operation-confirm t
  "If nil, do not prompt to confirm an operation."
  :type 'boolean
  :group 'guix)

(defcustom guix-use-substitutes t
  "If non-nil, use substitutes for the Guix packages."
  :type 'boolean
  :group 'guix)

(defvar guix-dry-run nil
  "If non-nil, do not perform the real actions, just simulate.")

(defvar guix-temp-buffer-name " *Guix temp*"
  "Name of a buffer used for displaying info before executing operation.")

(defvar guix-operation-option-true-string "yes"
  "String displayed in the mode-line when operation option is t.")

(defvar guix-operation-option-false-string "no "
  "String displayed in the mode-line when operation option is nil.")

(defvar guix-operation-option-separator "  |  "
  "String used in the mode-line to separate operation options.")

(defvar guix-operation-options
  '((?s "substitutes" guix-use-substitutes)
    (?d "dry-run"     guix-dry-run))
  "List of available operation options.
Each element of the list has a form:

  (KEY NAME VARIABLE)

KEY is a character that may be pressed during confirmation to
toggle the option.
NAME is a string displayed in the mode-line.
VARIABLE is a name of an option variable.")

(defun guix-operation-option-by-key (key)
  "Return operation option by KEY (character)."
  (assq key guix-operation-options))

(defun guix-operation-option-key (option)
  "Return key (character) of the operation OPTION."
  (car option))

(defun guix-operation-option-name (option)
  "Return name of the operation OPTION."
  (nth 1 option))

(defun guix-operation-option-variable (option)
  "Return name of the variable of the operation OPTION."
  (nth 2 option))

(defun guix-operation-option-value (option)
  "Return boolean value of the operation OPTION."
  (symbol-value (guix-operation-option-variable option)))

(defun guix-operation-option-string-value (option)
  "Convert boolean value of the operation OPTION to string and return it."
  (if (guix-operation-option-value option)
      guix-operation-option-true-string
    guix-operation-option-false-string))

(defun guix-operation-prompt (&optional prompt)
  "Prompt a user for continuing the current operation.
Return non-nil, if the operation should be continued; nil otherwise.
Ask a user with PROMPT for continuing an operation."
  (let* ((option-keys (mapcar #'guix-operation-option-key
                              guix-operation-options))
         (keys (append '(?y ?n) option-keys))
         (prompt (concat (propertize (or prompt "Continue operation?")
                                     'face 'minibuffer-prompt)
                         " ("
                         (mapconcat
                          (lambda (key)
                            (propertize (string key)
                                        'face 'guix-operation-option-key))
                          keys
                          ", ")
                         ") ")))
    (let ((mode-line mode-line-format))
      (prog1 (guix-operation-prompt-1 prompt keys)
        (setq mode-line-format mode-line)
        ;; Clear the minibuffer after prompting.
        (message "")))))

(defun guix-operation-prompt-1 (prompt keys)
  "This function is internal for `guix-operation-prompt'."
  (guix-operation-set-mode-line)
  (let ((key (read-char-choice prompt (cons ?\C-g keys) t)))
    (cl-case key
      (?y t)
      ((?n ?\C-g) nil)
      (t (let* ((option (guix-operation-option-by-key key))
                (var (guix-operation-option-variable option)))
           (set var (not (symbol-value var)))
           (guix-operation-prompt-1 prompt keys))))))

(defun guix-operation-set-mode-line ()
  "Display operation options in the mode-line of the current buffer."
  (setq mode-line-format
        (concat (propertize " Options:   "
                            'face 'mode-line-buffer-id)
                (mapconcat
                 (lambda (option)
                   (let ((key  (guix-operation-option-key option))
                         (name (guix-operation-option-name option))
                         (val  (guix-operation-option-string-value option)))
                     (concat name
                             " ("
                             (propertize (string key)
                                         'face 'guix-operation-option-key)
                             "): " val)))
                 guix-operation-options
                 guix-operation-option-separator)))
  (force-mode-line-update))

;;;###autoload
(defun guix-apply-manifest (profile file &optional operation-buffer)
  "Apply manifest from FILE to PROFILE.
This function has the same meaning as 'guix package --manifest' command.
See Info node `(guix) Invoking guix package' for details.

Interactively, use the current profile and prompt for manifest
FILE.  With a prefix argument, also prompt for PROFILE."
  (interactive
   (let* ((current-profile (guix-ui-current-profile))
          (profile (if current-prefix-arg
                       (guix-read-package-profile)
                     (or current-profile guix-current-profile)))
          (file (guix-read-file-name "File with manifest: "))
          (buffer (and current-profile (current-buffer))))
     (list profile file buffer)))
  (guix-assert-non-system-profile profile)
  (when (or (not guix-operation-confirm)
            (y-or-n-p (format "Apply manifest from '%s' to profile '%s'? "
                              file profile)))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'guix-command
      "package"
      (concat "--profile="  (expand-file-name profile))
      (concat "--manifest=" (expand-file-name file)))
     operation-buffer)))


;;; Executing guix commands

(defcustom guix-run-in-shell-function #'guix-run-in-shell
  "Function used to run guix command.
The function is called with a single argument - a command line string."
  :type '(choice (function-item guix-run-in-shell)
                 (function-item guix-run-in-eshell)
                 (function :tag "Other function"))
  :group 'guix)

(defcustom guix-shell-buffer-name "*shell*"
  "Default name of a shell buffer used for running guix commands."
  :type 'string
  :group 'guix)

(declare-function comint-send-input "comint" t)

(defun guix-run-in-shell (string)
  "Run command line STRING in `guix-shell-buffer-name' buffer."
  (shell guix-shell-buffer-name)
  (goto-char (point-max))
  (insert string)
  (comint-send-input))

(declare-function eshell-send-input "esh-mode" t)

(defun guix-run-in-eshell (string)
  "Run command line STRING in eshell buffer."
  (eshell)
  (goto-char (point-max))
  (insert string)
  (eshell-send-input))

(defun guix-run-command-in-shell (args)
  "Execute 'guix ARGS ...' command in a shell buffer."
  (funcall guix-run-in-shell-function
           (guix-command-string args)))

(defun guix-run-command-in-repl (args)
  "Execute 'guix ARGS ...' command in Guix REPL."
  (guix-eval-in-repl
   (apply #'guix-make-guile-expression
          'guix-command args)))

(defun guix-command-output (args)
  "Return string with 'guix ARGS ...' output."
  (cl-multiple-value-bind (output error)
      (guix-eval (apply #'guix-make-guile-expression
                        'guix-command-output args))
    ;; Remove trailing new space from the error string.
    (message (replace-regexp-in-string "\n\\'" "" (read error)))
    (read output)))

(defun guix-help-string (&optional commands)
  "Return string with 'guix COMMANDS ... --help' output."
  (guix-eval-read
   (apply #'guix-make-guile-expression
          'help-string commands)))


;;; Pull

(defcustom guix-update-after-pull t
  "If non-nil, update Guix buffers after performing \\[guix-pull]."
  :type 'boolean
  :group 'guix)

(defvar guix-after-pull-hook
  '(guix-restart-repl-after-pull guix-update-buffers-maybe-after-pull)
  "Hook run after successful performing `guix-pull' operation.")

(defun guix-restart-repl-after-pull ()
  "Restart Guix REPL after `guix-pull' operation."
  (guix-repl-exit)
  (guix-start-process-maybe
   "Restarting Guix REPL after pull operation ..."))

(defun guix-update-buffers-maybe-after-pull ()
  "Update buffers depending on `guix-update-after-pull'."
  (when guix-update-after-pull
    ;; No need to update "generation" buffers.
    (dolist (buffer (guix-operation-buffers
                     '(guix-package-list-mode
                       guix-package-info-mode
                       guix-output-list-mode)))
      (with-current-buffer buffer
        (revert-buffer nil t)))
    (message "Guix buffers have been updated.")))

;;;###autoload
(defun guix-pull (&optional verbose)
  "Run Guix pull operation.
If VERBOSE is non-nil (with prefix argument), produce verbose output."
  (interactive "P")
  (let ((args (and verbose '("--verbose"))))
    (guix-eval-in-repl
     (apply #'guix-make-guile-expression
            'guix-command "pull" args)
     nil 'pull)))

(provide 'guix-misc)

;;; guix-misc.el ends here
