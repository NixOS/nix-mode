;;; nix-hydra-build.el --- Interface for Hydra builds  -*- lexical-binding: t -*-

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

;; This file provides an interface for displaying Hydra builds in
;; 'list' and 'info' buffers.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'nix)
(require 'nix-hydra)
(require 'nix-utils)

(nix-hydra-define-entry-type build
  :search-types '((latest . nix-hydra-build-latest-api-url)
                  (queue  . nix-hydra-build-queue-api-url))
  :filters '(nix-hydra-build-filter-status)
  :filter-names '((nixname . name)
                  (buildstatus . build-status)
                  (timestamp . time))
  :filter-boolean-params '(finished busy))

(defun nix-hydra-build-get-display (search-type &rest args)
  "Search for Hydra builds and show results."
  (apply #'bui-list-get-display-entries
         'nix-hydra-build search-type args))

(cl-defun nix-hydra-build-latest-prompt-args (&key project jobset
                                                   job system)
  "Prompt for and return a list of 'latest builds' arguments."
  (let* ((number      (read-number "Number of latest builds: "))
         (project     (if current-prefix-arg
                          (nix-hydra-read-project nil project)
                        project))
         (jobset      (if current-prefix-arg
                          (nix-hydra-read-jobset nil jobset)
                        jobset))
         (job-or-name (if current-prefix-arg
                          (nix-hydra-read-job nil job)
                        job))
         (job         (and job-or-name
                           (string-match-p nix-hydra-job-regexp
                                           job-or-name)
                           job-or-name))
         (system      (if (and (not job)
                               (or current-prefix-arg
                                   (and job-or-name (not system))))
                          (if job-or-name
                              (guix-while-null
                               (guix-hydra-read-system
                                (concat job-or-name ".") system))
                            (guix-hydra-read-system nil system))
                        system))
         (job         (or job
                          (and job-or-name
                               (concat job-or-name "." system)))))
    (list number
          :project project
          :jobset  jobset
          :job     job
          :system  system)))

(declare-function nix-build-log-find-file "nix-build-log" (file))

(defun nix-hydra-build-view-log (id)
  "View build log of a hydra build ID."
  (require 'nix-build-log)
  (nix-build-log-find-file (nix-hydra-build-log-url id)))

;;; Defining URLs

(defun nix-hydra-build-url (id)
  "Return Hydra URL of a build ID."
  (nix-hydra-url "build/" (number-to-string id)))

(defun nix-hydra-build-log-url (id)
  "Return Hydra URL of the log file of a build ID."
  (concat (nix-hydra-build-url id) "/log/raw"))

(cl-defun nix-hydra-build-latest-api-url
    (number &key project jobset job system)
  "Return Hydra API URL to receive latest NUMBER of builds."
  (nix-hydra-api-url "latestbuilds"
    `(("nr" . ,number)
      ("project" . ,project)
      ("jobset" . ,jobset)
      ("job" . ,job)
      ("system" . ,system))))

(defun nix-hydra-build-queue-api-url (number)
  "Return Hydra API URL to receive the NUMBER of queued builds."
  (nix-hydra-api-url "queue"
    `(("nr" . ,number))))

;;; Filters for processing raw entries

(defun nix-hydra-build-filter-status (entry)
  "Add 'status' parameter to 'hydra-build' ENTRY."
  (let ((status (if (bui-entry-non-void-value entry 'finished)
                    (nix-hydra-build-status-number->name
                     (bui-entry-non-void-value entry 'build-status))
                  (if (bui-entry-non-void-value entry 'busy)
                      'running
                    'scheduled))))
    (cons `(status . ,status)
          entry)))

;;; Build status

(defface nix-hydra-build-status-running
  '((t :inherit bold))
  "Face used if hydra build is not finished."
  :group 'nix-hydra-build-faces)

(defface nix-hydra-build-status-scheduled
  '((t))
  "Face used if hydra build is scheduled."
  :group 'nix-hydra-build-faces)

(defface nix-hydra-build-status-succeeded
  '((t :inherit success))
  "Face used if hydra build succeeded."
  :group 'nix-hydra-build-faces)

(defface nix-hydra-build-status-cancelled
  '((t :inherit warning))
  "Face used if hydra build was cancelled."
  :group 'nix-hydra-build-faces)

(defface nix-hydra-build-status-failed
  '((t :inherit error))
  "Face used if hydra build failed."
  :group 'nix-hydra-build-faces)

(defvar nix-hydra-build-status-alist
  '((0 . succeeded)
    (1 . failed-build)
    (2 . failed-dependency)
    (3 . failed-other)
    (4 . cancelled))
  "Alist of hydra build status numbers and status names.
Status numbers are returned by Hydra API, names (symbols) are
used internally by the elisp code of this package.")

(defun nix-hydra-build-status-number->name (number)
  "Convert build status number to a name.
See `nix-hydra-build-status-alist'."
  (bui-assq-value nix-hydra-build-status-alist number))

(defun nix-hydra-build-status-string (status)
  "Return a human readable string for build STATUS."
  (cl-case status
    (scheduled
     (bui-get-string "Scheduled" 'nix-hydra-build-status-scheduled))
    (running
     (bui-get-string "Running" 'nix-hydra-build-status-running))
    (succeeded
     (bui-get-string "Succeeded" 'nix-hydra-build-status-succeeded))
    (cancelled
     (bui-get-string "Cancelled" 'nix-hydra-build-status-cancelled))
    (failed-build
     (nix-hydra-build-status-fail-string))
    (failed-dependency
     (nix-hydra-build-status-fail-string "dependency"))
    (failed-other
     (nix-hydra-build-status-fail-string "other"))))

(defun nix-hydra-build-status-fail-string (&optional reason)
  "Return a string for a failed build."
  (let ((base (bui-get-string "Failed" 'nix-hydra-build-status-failed)))
    (if reason
        (concat base " (" reason ")")
      base)))

(defun nix-hydra-build-finished? (entry)
  "Return non-nil, if hydra build was finished."
  (bui-entry-non-void-value entry 'finished))

(defun nix-hydra-build-running? (entry)
  "Return non-nil, if hydra build is running."
  (eq (bui-entry-non-void-value entry 'status)
      'running))

(defun nix-hydra-build-scheduled? (entry)
  "Return non-nil, if hydra build is scheduled."
  (eq (bui-entry-non-void-value entry 'status)
      'scheduled))

(defun nix-hydra-build-succeeded? (entry)
  "Return non-nil, if hydra build succeeded."
  (eq (bui-entry-non-void-value entry 'status)
      'succeeded))

(defun nix-hydra-build-cancelled? (entry)
  "Return non-nil, if hydra build was cancelled."
  (eq (bui-entry-non-void-value entry 'status)
      'cancelled))

(defun nix-hydra-build-failed? (entry)
  "Return non-nil, if hydra build failed."
  (memq (bui-entry-non-void-value entry 'status)
        '(failed-build failed-dependency failed-other)))

;;; Hydra build 'info'

(nix-hydra-define-interface build info
  :mode-name "Hydra-Build-Info"
  :buffer-name "*Hydra Build Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            nix-hydra-build-info-insert-url
            (time     format (time))
            (status   format nix-hydra-build-info-insert-status)
            (project  format (format nix-hydra-build-project))
            (jobset   format (format nix-hydra-build-jobset))
            (job      format (format nix-hydra-build-job))
            (system   format (format nix-hydra-build-system))
            (priority format (format))))

(defface nix-hydra-build-info-project
  '((t :inherit link))
  "Face for project names."
  :group 'nix-hydra-build-info-faces)

(defface nix-hydra-build-info-jobset
  '((t :inherit link))
  "Face for jobsets."
  :group 'nix-hydra-build-info-faces)

(defface nix-hydra-build-info-job
  '((t :inherit link))
  "Face for jobs."
  :group 'nix-hydra-build-info-faces)

(defface nix-hydra-build-info-system
  '((t :inherit link))
  "Face for system names."
  :group 'nix-hydra-build-info-faces)

(defmacro nix-hydra-build-define-button (name)
  "Define `nix-hydra-build-NAME' button."
  (let* ((name-str    (symbol-name name))
         (button-name (intern (concat "nix-hydra-build-" name-str)))
         (face-name   (intern (concat "nix-hydra-build-info-" name-str)))
         (keyword     (intern (concat ":" name-str))))
    `(define-button-type ',button-name
       :supertype 'bui
       'face ',face-name
       'help-echo ,(format "\
Show latest builds for this %s (with prefix, prompt for all parameters)"
                           name-str)
       'action (lambda (btn)
                 (let ((args (nix-hydra-build-latest-prompt-args
                              ,keyword (button-label btn))))
                   (apply #'nix-hydra-build-get-display
                          'latest args))))))

(nix-hydra-build-define-button project)
(nix-hydra-build-define-button jobset)
(nix-hydra-build-define-button job)
(nix-hydra-build-define-button system)

(defun nix-hydra-build-info-insert-url (entry)
  "Insert Hydra URL for the build ENTRY."
  (bui-insert-button (nix-hydra-build-url (bui-entry-id entry))
                     'bui-url)
  (when (nix-hydra-build-finished? entry)
    (bui-insert-indent)
    (bui-insert-action-button
     "Build log"
     (lambda (btn)
       (nix-hydra-build-view-log (button-get btn 'id)))
     "View build log"
     'id (bui-entry-id entry)))
  (bui-newline))

(defun nix-hydra-build-info-insert-status (status &optional _)
  "Insert a string with build STATUS."
  (insert (nix-hydra-build-status-string status)))

;;; Hydra build 'list'

(nix-hydra-define-interface build list
  :describe-function 'nix-hydra-list-describe
  :mode-name "Hydra-Build-List"
  :buffer-name "*Nix Hydra Builds*"
  :format '((name nil 30 t)
            (system nil 16 t)
            (status nix-hydra-build-list-get-status 20 t)
            (project nil 10 t)
            (jobset nil 17 t)
            (time bui-list-get-time 20 t))
  :hint 'nix-hydra-build-list-hint)

(let ((map nix-hydra-build-list-mode-map))
  (define-key map (kbd "B") 'nix-hydra-build-list-latest-builds)
  (define-key map (kbd "L") 'nix-hydra-build-list-view-log))

(defvar nix-hydra-build-list-default-hint
  '(("\\[nix-hydra-build-list-latest-builds]")
    " show latest builds of the current job;\n"
    ("\\[nix-hydra-build-list-view-log]") " show build log;\n"))

(defun nix-hydra-build-list-hint ()
  (bui-format-hints
   nix-hydra-build-list-default-hint
   (bui-default-hint)))

(defun nix-hydra-build-list-get-status (status &optional _)
  "Return a string for build STATUS."
  (nix-hydra-build-status-string status))

(defun nix-hydra-build-list-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds of the current job.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (nix-hydra-build-latest-prompt-args
      :project (bui-entry-non-void-value entry 'project)
      :jobset  (bui-entry-non-void-value entry 'name)
      :job     (bui-entry-non-void-value entry 'job)
      :system  (bui-entry-non-void-value entry 'system))))
  (apply #'nix-hydra-latest-builds number args))

(defun nix-hydra-build-list-view-log ()
  "View build log of the current Hydra build."
  (interactive)
  (nix-hydra-build-view-log (bui-list-current-id)))

;;; Interactive commands

;;;###autoload
(defun nix-hydra-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds.
ARGS are the same arguments as for `nix-hydra-build-latest-api-url'.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive (nix-hydra-build-latest-prompt-args))
  (apply #'nix-hydra-build-get-display
         'latest number args))

;;;###autoload
(defun nix-hydra-queued-builds (number)
  "Display the NUMBER of queued Hydra builds."
  (interactive "NNumber of queued builds: ")
  (nix-hydra-build-get-display 'queue number))

(provide 'nix-hydra-build)

;;; nix-hydra-build.el ends here
