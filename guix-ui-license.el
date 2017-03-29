;;; guix-ui-license.el --- Interface for displaying licenses  -*- lexical-binding: t -*-

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

;; This file provides 'list'/'info' interface for displaying licenses of
;; Guix packages.

;;; Code:

(require 'bui)
(require 'guix nil t)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-license)
(require 'guix-utils)

(guix-define-groups license)

(bui-define-entry-type guix-license
  :message-function 'guix-license-message
  :titles '((url . "URL")))

(defun guix-license-get-entries (search-type &rest args)
  "Receive 'license' entries.
SEARCH-TYPE may be one of the following symbols: `all', `id', `name'."
  (guix-eval-read
   (apply #'guix-make-guile-expression
          'license-sexps search-type args)))

(defun guix-license-get-display (search-type &rest args)
  "Search for licenses and show results."
  (apply #'bui-list-get-display-entries
         'guix-license search-type args))

(defun guix-license-message (entries _search-type &rest args)
  "Display a message after showing license ENTRIES."
  ;; Some objects in (guix licenses) module are procedures (e.g.,
  ;; 'non-copyleft' or 'x11-style').  Such licenses cannot be "described".
  (when (null entries)
    (if (cdr args)
        (message "Unknown licenses.")
      (message "Unknown license."))))


;;; License 'info'

(bui-define-interface guix-license info
  :mode-name "License-Info"
  :buffer-name "*Guix License Info*"
  :get-entries-function 'guix-license-get-entries
  :format '((name nil (simple bui-info-heading))
            nil
            guix-license-insert-packages-button
            (url nil (simple bui-url))
            guix-license-insert-comment
            nil
            guix-license-insert-file))

(declare-function guix-packages-by-license "guix-ui-package" t)

(defun guix-license-insert-packages-button (entry)
  "Insert button to display packages by license ENTRY."
  (let ((license (bui-entry-value entry 'name)))
    (bui-insert-action-button
     "Packages"
     (lambda (btn)
       (guix-packages-by-license (button-get btn 'license)))
     (format "Display packages with license '%s'" license)
     'license license))
  (bui-newline))

(defun guix-license-insert-comment (entry)
  "Insert 'comment' of a license ENTRY."
  (let ((comment (bui-entry-value entry 'comment)))
    (if (and comment
             (string-match-p "^http" comment))
        (bui-info-insert-value-simple comment 'bui-url)
      (bui-info-insert-title-simple (bui-current-param-title 'comment))
      (bui-info-insert-value-indent comment)))
  (bui-newline))

(defun guix-license-insert-file (entry)
  "Insert button to open license definition."
  (let ((license (bui-entry-value entry 'name)))
    (bui-insert-button
     (guix-license-file) 'bui-file
     'help-echo (format "Open definition of license '%s'" license)
     'action (lambda (btn)
               (guix-find-license-definition (button-get btn 'license)))
     'license license))
  (bui-newline))


;;; License 'list'

(bui-define-interface guix-license list
  :mode-name "License-List"
  :buffer-name "*Guix Licenses*"
  :get-entries-function 'guix-license-get-entries
  :describe-function 'guix-license-list-describe
  :format '((name nil 40 t)
            (url bui-list-get-url 50 t))
  :titles '((name . "License"))
  :hint 'guix-license-list-hint
  :sort-key '(name))

(let ((map guix-license-list-mode-map))
  (define-key map (kbd "e")   'guix-license-list-edit)
  (define-key map (kbd "P")   'guix-license-list-show-packages))

(defvar guix-license-list-default-hint
  '(("\\[guix-license-list-show-packages]") " show packages;\n"
    ("\\[guix-license-list-edit]") " edit (go to) the license definition;\n"))

(defun guix-license-list-hint ()
  (bui-format-hints
   guix-license-list-default-hint
   (bui-list-hint)
   bui-common-hint))

(defun guix-license-list-describe (&rest ids)
  "Describe licenses with IDS (list of identifiers)."
  (bui-display-entries
   (bui-entries-by-ids (bui-current-entries) ids)
   'guix-license 'info (cons 'id ids)))

(defun guix-license-list-show-packages ()
  "Display packages with the license at point."
  (interactive)
  (guix-packages-by-license (bui-list-current-id)))

(defun guix-license-list-edit (&optional directory)
  "Go to the location of the current license definition.
See `guix-license-file' for the meaning of DIRECTORY."
  (interactive (list (guix-read-directory)))
  (guix-find-license-definition (bui-list-current-id) directory))


;;; Interactive commands

(defun guix-licenses-show ()
  "Display licenses of the Guix packages.
Unlike `guix-licenses', this command always recreates
`guix-license-list-buffer-name' buffer."
  (interactive)
  (guix-license-get-display 'all))

;;;###autoload
(defun guix-licenses ()
  "Display licenses of the Guix packages.
Switch to the `guix-license-list-buffer-name' buffer if it
already exists."
  (interactive)
  (guix-switch-to-buffer-or-funcall
   guix-license-list-buffer-name #'guix-licenses-show 'message))

(provide 'guix-ui-license)

;;; guix-ui-license.el ends here
