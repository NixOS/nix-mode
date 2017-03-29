;;; guix-ui-system-generation.el --- Interface for displaying system generations  -*- lexical-binding: t -*-

;; Copyright © 2016–2017 Alex Kost <alezost@gmail.com>

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

;; This file provides an interface for displaying system generations
;; in 'list' and 'info' buffers, and commands for working with them.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'guix nil t)
(require 'guix-ui)
(require 'guix-ui-generation)
(require 'guix-utils)
(require 'guix-profiles)

(guix-ui-define-entry-type system-generation)

(defun guix-system-generation-add-kernel-config (entry)
  "Return ENTRY with 'kernel-config' parameter."
  (let* ((kernel (bui-entry-value entry 'kernel))
         (dir    (file-name-directory kernel))
         ;; Nowadays kernel config has ".config" name, but before
         ;; <http://git.savannah.gnu.org/cgit/guix.git/commit/?id=e4e9c0a083962a770ff4f56c69082cf4b7046a6c>
         ;; it was "config" (without "."), so find the right name.
         (config (car (directory-files dir 'full-name
                                       "config\\'" 'no-sort))))
    `((kernel-config . ,config)
      ,@entry)))

(defun guix-system-generation-add-shepherd-config (entry)
  "Return ENTRY with 'shepherd-config' parameter."
  (let* ((file-name (bui-entry-value entry 'file-name))
         (boot-file (expand-file-name "boot" file-name)))
    (with-temp-buffer
      (insert-file-contents-literally boot-file)
      (goto-char (point-min))
      (if (re-search-forward (rx "/gnu/store/" (+ alnum)
                                 "-shepherd.conf")
                             nil t)
          `((shepherd-config . ,(match-string 0))
            ,@entry)
        entry))))

(defun guix-system-generation-get-entries (profile search-type
                                                   search-values params)
  "Return 'system-generation' entries."
  (let* ((add-kernel-config? (or (null params)
                                 (memq 'kernel-config params)))
         (add-shepherd-config? (or (null params)
                                   (memq 'shepherd-config params)))
         (params (if (and add-kernel-config?
                          (not (memq 'kernel params)))
                     (cons 'kernel params)
                   params))
         (params (if (and add-shepherd-config?
                          (not (memq 'file-name params)))
                     (cons 'file-name params)
                   params)))
    (apply #'guix-modify-objects
           (guix-generation-get-entries
            'system-generation-sexps
            profile search-type search-values params)
           (delq nil
                 (list
                  (and add-shepherd-config?
                       #'guix-system-generation-add-shepherd-config)
                  (and add-kernel-config?
                       #'guix-system-generation-add-kernel-config))))))

(defun guix-system-generation-get-display (search-type &rest search-values)
  "Search for system generations and show results.
See `guix-ui-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALUES."
  (apply #'bui-list-get-display-entries
         'guix-system-generation
         guix-system-profile
         search-type search-values))


;;; System generation 'info'

(guix-ui-define-interface system-generation info
  :mode-name "System-Generation-Info"
  :buffer-name "*Guix Generation Info*"
  :get-entries-function 'guix-system-generation-info-get-entries
  :format '(guix-generation-info-insert-heading
            nil
            (label format (format))
            (prev-number format guix-generation-info-insert-previous)
            (current format guix-generation-info-insert-current)
            (number-of-packages format guix-generation-info-insert-packages)
            (file-name format (format bui-file))
            (time format (time))
            (root-device format (format))
            (store-device format (format))
            (store-mount-point format (format))
            (kernel-arguments format (format))
            (kernel-config simple (indent bui-file))
            (shepherd-config simple
                             guix-system-generation-info-insert-shepherd))
  :titles guix-generation-info-titles
  :required guix-generation-info-required-params)

(defun guix-system-generation-info-get-entries (profile search-type
                                                        &rest search-values)
  "Return 'system-generation' entries for displaying them in 'info' buffer."
  (guix-system-generation-get-entries
   profile search-type search-values
   (cl-union guix-system-generation-info-required-params
             (bui-info-displayed-params 'guix-system-generation))))

(defun guix-system-generation-pretty-print (config)
  "Pretty print Sheprhed CONFIG file name."
  (let* ((base-name (file-name-nondirectory config))
         (buffer    (generate-new-buffer base-name)))
    (with-current-buffer buffer
      (insert-file-contents config)
      (goto-char (point-min))
      ;; Put "/gnu/store/..." strings and service names on the new
      ;; lines.
      (save-excursion
        (while (re-search-forward "(quote" nil t)
          (down-list)
          (forward-sexp)
          (while (not (looking-at ")"))
            (insert "\n")
            (forward-sexp))))
      (scheme-mode))
    (guix-pretty-print-buffer buffer)
    (switch-to-buffer buffer)))

(defun guix-system-generation-info-insert-shepherd (config &optional _)
  "Insert Shepherd CONFIG file name and 'Pretty print' button at point."
  (bui-insert-action-button
   "Pretty print"
   (lambda (btn)
     (guix-system-generation-pretty-print (button-get btn 'config)))
   "Show Shepherd config in a human-readable form"
   'config config)
  (bui-info-insert-value-indent config 'bui-file))


;;; System generation 'list'

;; FIXME It is better to make `guix-generation-list-shared-map' with
;; common keys for both usual and system generations.
(defvar guix-system-generation-list-mode-map
  (copy-keymap guix-generation-list-mode-map)
  "Keymap for `guix-system-generation-list-mode' buffers.")

(guix-ui-define-interface system-generation list
  :mode-name "System-Generation-List"
  :buffer-name "*Guix Generations*"
  :get-entries-function 'guix-system-generation-list-get-entries
  :describe-function 'guix-ui-list-describe
  :format '((number nil 5 bui-list-sort-numerically-0 :right-align t)
            (current guix-generation-list-get-current 10 t)
            (label nil 35 t)
            (number-of-packages nil 11 bui-list-sort-numerically-3
                                :right-align t)
            (time bui-list-get-time 20 t))
  :titles guix-generation-list-titles
  :hint guix-generation-list-hint
  :sort-key guix-generation-list-sort-key
  :marks guix-generation-list-additional-marks)

(defun guix-system-generation-list-get-entries (profile search-type
                                                        &rest search-values)
  "Return 'system-generation' entries for displaying them in 'list' buffer."
  (guix-system-generation-get-entries
   profile search-type search-values
   (cl-union guix-system-generation-list-required-params
             (bui-list-displayed-params 'guix-system-generation))))


;;; Interactive commands

;;;###autoload
(defun guix-system-generations ()
  "Display information about system generations."
  (interactive)
  (guix-system-generation-get-display 'all))

;;;###autoload
(defun guix-last-system-generations (number)
  "Display information about last NUMBER of system generations."
  (interactive "nThe number of last generations: ")
  (guix-system-generation-get-display 'last number))

;;;###autoload
(defun guix-system-generations-by-time (from to)
  "Display information about system generations created between FROM and TO."
  (interactive
   (list (guix-read-date "Find generations (from): ")
         (guix-read-date "Find generations (to): ")))
  (guix-system-generation-get-display
   'time (float-time from) (float-time to)))

(provide 'guix-ui-system-generation)

;;; guix-ui-system-generation.el ends here
