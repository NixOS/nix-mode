;;; guix-hydra-jobset.el --- Interface for Hydra jobsets  -*- lexical-binding: t -*-

;; Copyright © 2015–2017 Alex Kost <alezost@gmail.com>

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

;; This file provides an interface for displaying Hydra jobsets in
;; 'list' and 'info' buffers.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'nix-hydra)
(require 'nix-hydra-build)

(nix-hydra-define-entry-type jobset
  :search-types '((project . nix-hydra-jobset-api-url))
  :filters '(nix-hydra-jobset-filter-id)
  :filter-names '((nrscheduled . scheduled)
                  (nrsucceeded . succeeded)
                  (nrfailed . failed)
                  (nrtotal . total)))

(defun nix-hydra-jobset-get-display (search-type &rest args)
  "Search for Hydra builds and show results."
  (apply #'bui-list-get-display-entries
         'nix-hydra-jobset search-type args))

;;; Defining URLs

(defun nix-hydra-jobset-url (project jobset)
  "Return Hydra URL of a PROJECT's JOBSET."
  (nix-hydra-url "jobset/" project "/" jobset))

(defun nix-hydra-jobset-api-url (project)
  "Return Hydra API URL for jobsets by PROJECT."
  (nix-hydra-api-url "jobsets"
    `(("project" . ,project))))

;;; Filters for processing raw entries

(defun nix-hydra-jobset-filter-id (entry)
  "Add 'ID' parameter to 'hydra-jobset' ENTRY."
  (cons `(id . ,(bui-entry-non-void-value entry 'name))
        entry))

;;; Hydra jobset 'info'

(nix-hydra-define-interface jobset info
  :mode-name "Hydra-Jobset-Info"
  :buffer-name "*Guix Hydra Jobset Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            nix-hydra-jobset-info-insert-url
            (project   format nix-hydra-jobset-info-insert-project)
            (scheduled format (format nix-hydra-jobset-info-scheduled))
            (succeeded format (format nix-hydra-jobset-info-succeeded))
            (failed    format (format nix-hydra-jobset-info-failed))
            (total     format (format nix-hydra-jobset-info-total))))

(defface nix-hydra-jobset-info-scheduled
  '((t))
  "Face used for the number of scheduled builds."
  :group 'nix-hydra-jobset-info-faces)

(defface nix-hydra-jobset-info-succeeded
  '((t :inherit nix-hydra-build-status-succeeded))
  "Face used for the number of succeeded builds."
  :group 'nix-hydra-jobset-info-faces)

(defface nix-hydra-jobset-info-failed
  '((t :inherit nix-hydra-build-status-failed))
  "Face used for the number of failed builds."
  :group 'nix-hydra-jobset-info-faces)

(defface nix-hydra-jobset-info-total
  '((t))
  "Face used for the total number of builds."
  :group 'nix-hydra-jobset-info-faces)

(defun nix-hydra-jobset-info-insert-project (project entry)
  "Insert PROJECT button for the jobset ENTRY."
  (let ((jobset (bui-entry-non-void-value entry 'name)))
    (bui-insert-button
     project 'nix-hydra-build-project
     'action (lambda (btn)
               (let ((args (nix-hydra-build-latest-prompt-args
                            :project (button-get btn 'project)
                            :jobset  (button-get btn 'jobset))))
                 (apply #'nix-hydra-build-get-display
                        'latest args)))
     'project project
     'jobset jobset)))

(defun nix-hydra-jobset-info-insert-url (entry)
  "Insert Hydra URL for the jobset ENTRY."
  (bui-insert-button (nix-hydra-jobset-url
                      (bui-entry-non-void-value entry 'project)
                      (bui-entry-non-void-value entry 'name))
                     'bui-url)
  (bui-newline))

;;; Hydra jobset 'list'

(nix-hydra-define-interface jobset list
  :describe-function 'nix-hydra-list-describe
  :mode-name "Hydra-Jobset-List"
  :buffer-name "*Guix Hydra Jobsets*"
  :format '((name nil 25 t)
            (project nil 10 t)
            (scheduled nil 12 t)
            (succeeded nil 12 t)
            (failed nil 9 t)
            (total nil 10 t))
  :hint 'nix-hydra-jobset-list-hint)

(let ((map nix-hydra-jobset-list-mode-map))
  (define-key map (kbd "B") 'nix-hydra-jobset-list-latest-builds))

(defvar nix-hydra-jobset-list-default-hint
  '(("\\[nix-hydra-jobset-list-latest-builds]")
    " show latest builds for the current jobset;\n"))

(defun nix-hydra-jobset-list-hint ()
  (bui-format-hints
   nix-hydra-jobset-list-default-hint
   (bui-default-hint)))

(defun nix-hydra-jobset-list-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds of the current jobset.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (nix-hydra-build-latest-prompt-args
      :project (bui-entry-non-void-value entry 'project)
      :jobset  (bui-entry-non-void-value entry 'name))))
  (apply #'nix-hydra-latest-builds number args))

;;; Interactive commands

;;;###autoload
(defun nix-hydra-jobsets (project)
  "Display jobsets of PROJECT."
  (interactive (list (nix-hydra-read-project)))
  (nix-hydra-jobset-get-display 'project project))

(provide 'nix-hydra-jobset)

;;; nix-hydra-jobset.el ends here
