;;; nix-build-log.el --- Major and minor modes for build logs  -*- lexical-binding: t -*-

;; Copyright © 2015 Alex Kost <alezost@gmail.com>

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

;; This file provides a major mode (`nix-build-log-mode') and a minor mode
;; (`nix-build-log-minor-mode') for highlighting Guix build logs.

;;; Code:

(require 'nix)
(require 'nix-utils)

(defgroup nix-build-log nil
  "Settings for `nix-build-log-mode'."
  :group 'guix)

(defgroup nix-build-log-faces nil
  "Faces for `nix-build-log-mode'."
  :group 'nix-build-log
  :group 'nix-faces)

(defface nix-build-log-title-head
  '((t :inherit font-lock-keyword-face))
  "Face for '@' symbol of a log title."
  :group 'nix-build-log-faces)

(defface nix-build-log-title-start
  '((t :inherit nix-build-log-title-head))
  "Face for a log title denoting a start of a process."
  :group 'nix-build-log-faces)

(defface nix-build-log-title-success
  '((t :inherit nix-build-log-title-head))
  "Face for a log title denoting a successful end of a process."
  :group 'nix-build-log-faces)

(defface nix-build-log-title-fail
  '((t :inherit error))
  "Face for a log title denoting a failed end of a process."
  :group 'nix-build-log-faces)

(defface nix-build-log-title-end
  '((t :inherit nix-build-log-title-head))
  "Face for a log title denoting an undefined end of a process."
  :group 'nix-build-log-faces)

(defface nix-build-log-phase-name
  '((t :inherit font-lock-function-name-face))
  "Face for a phase name."
  :group 'nix-build-log-faces)

(defface nix-build-log-phase-start
  '((default :weight bold)
    (((class grayscale) (background light)) :foreground "Gray90")
    (((class grayscale) (background dark))  :foreground "DimGray")
    (((class color) (min-colors 16) (background light))
     :foreground "DarkGreen")
    (((class color) (min-colors 16) (background dark))
     :foreground "LimeGreen")
    (((class color) (min-colors 8)) :foreground "green"))
  "Face for the start line of a phase."
  :group 'nix-build-log-faces)

(defface nix-build-log-phase-end
  '((((class grayscale) (background light)) :foreground "Gray90")
    (((class grayscale) (background dark))  :foreground "DimGray")
    (((class color) (min-colors 16) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 16) (background dark))
     :foreground "LightGreen")
    (((class color) (min-colors 8)) :foreground "green")
    (t :weight bold))
  "Face for the end line of a phase."
  :group 'nix-build-log-faces)

(defface nix-build-log-phase-success
  '((t))
  "Face for the 'succeeded' word of a phase line."
  :group 'nix-build-log-faces)

(defface nix-build-log-phase-fail
  '((t :inherit error))
  "Face for the 'failed' word of a phase line."
  :group 'nix-build-log-faces)

(defface nix-build-log-phase-seconds
  '((t :inherit font-lock-constant-face))
  "Face for the number of seconds for a phase."
  :group 'nix-build-log-faces)

(defcustom nix-build-log-mode-hook '()
  "Hook run after `nix-build-log-mode' is entered."
  :type 'hook
  :group 'nix-build-log)

(defvar nix-build-log-phase-name-regexp "`\\([^']+\\)'"
  "Regexp for a phase name.")

(defvar nix-build-log-phase-start-regexp
  (concat "^starting phase " nix-build-log-phase-name-regexp)
  "Regexp for the start line of a 'build' phase.")

(defun nix-build-log-title-regexp (&optional state)
  "Return regexp for the log title.
STATE is a symbol denoting a state of the title.  It should be
`start', `fail', `success' or `nil' (for a regexp matching any
state)."
  (let* ((word-rx (rx (1+ (any word "-"))))
         (state-rx (cond ((eq state 'start)   (concat word-rx "started"))
                         ((eq state 'success) (concat word-rx "succeeded"))
                         ((eq state 'fail)    (concat word-rx "failed"))
                         (t word-rx))))
    (rx-to-string
     `(and bol (group "@") " " (group (regexp ,state-rx)))
     t)))

(defun nix-build-log-phase-end-regexp (&optional state)
  "Return regexp for the end line of a 'build' phase.
STATE is a symbol denoting how a build phase was ended.  It should be
`fail', `success' or `nil' (for a regexp matching any state)."
  (let ((state-rx (cond ((eq state 'success) "succeeded")
                        ((eq state 'fail)    "failed")
                        (t (regexp-opt '("succeeded" "failed"))))))
    (rx-to-string
     `(and bol "phase " (regexp ,nix-build-log-phase-name-regexp)
           " " (group (regexp ,state-rx)) " after "
           (group (1+ (or digit "."))) " seconds")
     t)))

(defvar nix-build-log-phase-end-regexp
  ;; For efficiency, it is better to have a regexp for the general line
  ;; of the phase end, then to call the function all the time.
  (nix-build-log-phase-end-regexp)
  "Regexp for the end line of a 'build' phase.")

(defvar nix-build-log-font-lock-keywords
  `((,(nix-build-log-title-regexp 'start)
     (1 'nix-build-log-title-head)
     (2 'nix-build-log-title-start))
    (,(nix-build-log-title-regexp 'success)
     (1 'nix-build-log-title-head)
     (2 'nix-build-log-title-success))
    (,(nix-build-log-title-regexp 'fail)
     (1 'nix-build-log-title-head)
     (2 'nix-build-log-title-fail))
    (,(nix-build-log-title-regexp)
     (1 'nix-build-log-title-head)
     (2 'nix-build-log-title-end))
    (,nix-build-log-phase-start-regexp
     (0 'nix-build-log-phase-start)
     (1 'nix-build-log-phase-name prepend))
    (,(nix-build-log-phase-end-regexp 'success)
     (0 'nix-build-log-phase-end)
     (1 'nix-build-log-phase-name prepend)
     (2 'nix-build-log-phase-success prepend)
     (3 'nix-build-log-phase-seconds prepend))
    (,(nix-build-log-phase-end-regexp 'fail)
     (0 'nix-build-log-phase-end)
     (1 'nix-build-log-phase-name prepend)
     (2 'nix-build-log-phase-fail prepend)
     (3 'nix-build-log-phase-seconds prepend)))
  "A list of `font-lock-keywords' for `nix-build-log-mode'.")

(defvar nix-build-log-common-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'nix-build-log-next-phase)
    (define-key map (kbd "M-p") 'nix-build-log-previous-phase)
    (define-key map (kbd "TAB") 'nix-build-log-phase-toggle)
    (define-key map (kbd "<tab>") 'nix-build-log-phase-toggle)
    (define-key map (kbd "<backtab>") 'nix-build-log-phase-toggle-all)
    (define-key map [(shift tab)] 'nix-build-log-phase-toggle-all)
    map)
  "Parent keymap for 'build-log' buffers.
For `nix-build-log-mode' this map is used as is.
For `nix-build-log-minor-mode' this map is prefixed with 'C-c'.")

(defvar nix-build-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap (list nix-build-log-common-map)
                               special-mode-map))
    (define-key map (kbd "c") 'compilation-shell-minor-mode)
    (define-key map (kbd "v") 'view-mode)
    map)
  "Keymap for `nix-build-log-mode' buffers.")

(defvar nix-build-log-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") nix-build-log-common-map)
    map)
  "Keymap for `nix-build-log-minor-mode' buffers.")

(defun nix-build-log-phase-start (&optional with-header?)
  "Return the start point of the current build phase.
If WITH-HEADER? is non-nil, do not skip 'starting phase ...' header.
Return nil, if there is no phase start before the current point."
  (save-excursion
    (end-of-line)
    (when (re-search-backward nix-build-log-phase-start-regexp nil t)
      (unless with-header? (end-of-line))
      (point))))

(defun nix-build-log-phase-end ()
  "Return the end point of the current build phase."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward nix-build-log-phase-end-regexp nil t)
      (point))))

(defun nix-build-log-phase-hide ()
  "Hide the body of the current build phase."
  (interactive)
  (let ((beg (nix-build-log-phase-start))
        (end (nix-build-log-phase-end)))
    (when (and beg end)
      ;; If not on the header line, move to it.
      (when (and (> (point) beg)
                 (< (point) end))
        (goto-char (nix-build-log-phase-start t)))
      (remove-overlays beg end 'invisible t)
      (let ((o (make-overlay beg end)))
        (overlay-put o 'evaporate t)
        (overlay-put o 'invisible t)))))

(defun nix-build-log-phase-show ()
  "Show the body of the current build phase."
  (interactive)
  (let ((beg (nix-build-log-phase-start))
        (end (nix-build-log-phase-end)))
    (when (and beg end)
      (remove-overlays beg end 'invisible t))))

(defun nix-build-log-phase-hidden-p ()
  "Return non-nil, if the body of the current build phase is hidden."
  (let ((beg (nix-build-log-phase-start)))
    (and beg
         (cl-some (lambda (o)
                    (overlay-get o 'invisible))
                  (overlays-at beg)))))

(defun nix-build-log-phase-toggle-function ()
  "Return a function to toggle the body of the current build phase."
  (if (nix-build-log-phase-hidden-p)
      #'nix-build-log-phase-show
    #'nix-build-log-phase-hide))

(defun nix-build-log-phase-toggle ()
  "Show/hide the body of the current build phase."
  (interactive)
  (funcall (nix-build-log-phase-toggle-function)))

(defun nix-build-log-phase-toggle-all ()
  "Show/hide the bodies of all build phases."
  (interactive)
  (save-excursion
    ;; Some phases may be hidden, and some shown.  Whether to hide or to
    ;; show them, it is determined by the state of the first phase here.
    (goto-char (point-min))
    (let ((fun (save-excursion
                 (re-search-forward nix-build-log-phase-start-regexp nil t)
                 (nix-build-log-phase-toggle-function))))
      (while (re-search-forward nix-build-log-phase-start-regexp nil t)
        (funcall fun)))))

(defun nix-build-log-next-phase (&optional arg)
  "Move to the next build phase.
With ARG, do it that many times.  Negative ARG means move
backward."
  (interactive "^p")
  (if arg
      (when (zerop arg) (user-error "Try again"))
    (setq arg 1))
  (let ((search-fun (if (> arg 0)
                        #'re-search-forward
                      #'re-search-backward))
        (n (abs arg))
        found last-found)
    (save-excursion
      (end-of-line (if (> arg 0) 1 0))  ; skip the current line
      (while (and (not (zerop n))
                  (setq found
                        (funcall search-fun
                                 nix-build-log-phase-start-regexp
                                 nil t)))
        (setq n (1- n)
              last-found found)))
    (when last-found
      (goto-char last-found)
      (forward-line 0))
    (or found
        (user-error (if (> arg 0)
                        "No next build phase"
                      "No previous build phase")))))

(defun nix-build-log-previous-phase (&optional arg)
  "Move to the previous build phase.
With ARG, do it that many times.  Negative ARG means move
forward."
  (interactive "^p")
  (nix-build-log-next-phase (- (or arg 1))))

;;;###autoload
(define-derived-mode nix-build-log-mode special-mode
  "Nix-Build-Log"
  "Major mode for viewing Guix build logs.

\\{nix-build-log-mode-map}"
  (setq font-lock-defaults '(nix-build-log-font-lock-keywords t)))

;;;###autoload
(define-minor-mode nix-build-log-minor-mode
  "Toggle Guix Build Log minor mode.

With a prefix argument ARG, enable Guix Build Log minor mode if
ARG is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When Guix Build Log minor mode is enabled, it highlights build
log in the current buffer.  This mode can be enabled
programmatically using hooks, like this:

  (add-hook 'shell-mode-hook 'nix-build-log-minor-mode)

\\{nix-build-log-minor-mode-map}"
  :init-value nil
  :lighter " Nix-Build-Log"
  :keymap nix-build-log-minor-mode-map
  :group 'nix-build-log
  (if nix-build-log-minor-mode
      (font-lock-add-keywords nil nix-build-log-font-lock-keywords)
    (font-lock-remove-keywords nil nix-build-log-font-lock-keywords))
  (nix-font-lock-flush))

(defun nix-build-log-find-file (file-or-url)
  "Open FILE-OR-URL in `nix-build-log-mode'."
  (nix-find-file-or-url file-or-url)
  (nix-build-log-mode))

;;;###autoload
(add-to-list 'auto-mode-alist
             ;; Regexp for log files (usually placed in /var/log/guix/...)
             (cons (rx "/guix/drvs/" (= 2 alnum) "/" (= 30 alnum)
                       "-" (+ (any alnum "-+.")) ".drv" string-end)
                   'nix-build-log-mode))

(provide 'nix-build-log)

;;; nix-build-log.el ends here
