;;; guix-hydra-build.el --- Interface for Hydra builds  -*- lexical-binding: t -*-

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
(require 'guix-hydra)
(require 'guix-utils)

(guix-hydra-define-entry-type build
  :search-types '((latest . guix-hydra-build-latest-api-url)
                  (queue  . guix-hydra-build-queue-api-url))
  :filters '(guix-hydra-build-filter-status)
  :filter-names '((nixname . name)
                  (buildstatus . build-status)
                  (timestamp . time))
  :filter-boolean-params '(finished busy))

(defun guix-hydra-build-get-display (search-type &rest args)
  "Search for Hydra builds and show results."
  (apply #'bui-list-get-display-entries
         'guix-hydra-build search-type args))

(cl-defun guix-hydra-build-latest-prompt-args (&key project jobset
                                                    job system)
  "Prompt for and return a list of 'latest builds' arguments."
  (let* ((number      (read-number "Number of latest builds: "))
         (project     (if current-prefix-arg
                          (guix-hydra-read-project nil project)
                        project))
         (jobset      (if current-prefix-arg
                          (guix-hydra-read-jobset nil jobset)
                        jobset))
         (job-or-name (if current-prefix-arg
                          (guix-hydra-read-job nil job)
                        job))
         (job         (and job-or-name
                           (string-match-p guix-hydra-job-regexp
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

(declare-function guix-build-log-find-file "guix-build-log" (file))

(defun guix-hydra-build-view-log (id)
  "View build log of a hydra build ID."
  (require 'guix-build-log)
  (guix-build-log-find-file (guix-hydra-build-log-url id)))


;;; Defining URLs

(defun guix-hydra-build-url (id)
  "Return Hydra URL of a build ID."
  (guix-hydra-url "build/" (number-to-string id)))

(defun guix-hydra-build-log-url (id)
  "Return Hydra URL of the log file of a build ID."
  (concat (guix-hydra-build-url id) "/log/raw"))

(cl-defun guix-hydra-build-latest-api-url
    (number &key project jobset job system)
  "Return Hydra API URL to receive latest NUMBER of builds."
  (guix-hydra-api-url "latestbuilds"
    `(("nr" . ,number)
      ("project" . ,project)
      ("jobset" . ,jobset)
      ("job" . ,job)
      ("system" . ,system))))

(defun guix-hydra-build-queue-api-url (number)
  "Return Hydra API URL to receive the NUMBER of queued builds."
  (guix-hydra-api-url "queue"
    `(("nr" . ,number))))


;;; Filters for processing raw entries

(defun guix-hydra-build-filter-status (entry)
  "Add 'status' parameter to 'hydra-build' ENTRY."
  (let ((status (if (bui-entry-non-void-value entry 'finished)
                    (guix-hydra-build-status-number->name
                     (bui-entry-non-void-value entry 'build-status))
                  (if (bui-entry-non-void-value entry 'busy)
                      'running
                    'scheduled))))
    (cons `(status . ,status)
          entry)))


;;; Build status

(defface guix-hydra-build-status-running
  '((t :inherit bold))
  "Face used if hydra build is not finished."
  :group 'guix-hydra-build-faces)

(defface guix-hydra-build-status-scheduled
  '((t))
  "Face used if hydra build is scheduled."
  :group 'guix-hydra-build-faces)

(defface guix-hydra-build-status-succeeded
  '((t :inherit success))
  "Face used if hydra build succeeded."
  :group 'guix-hydra-build-faces)

(defface guix-hydra-build-status-cancelled
  '((t :inherit warning))
  "Face used if hydra build was cancelled."
  :group 'guix-hydra-build-faces)

(defface guix-hydra-build-status-failed
  '((t :inherit error))
  "Face used if hydra build failed."
  :group 'guix-hydra-build-faces)

(defvar guix-hydra-build-status-alist
  '((0 . succeeded)
    (1 . failed-build)
    (2 . failed-dependency)
    (3 . failed-other)
    (4 . cancelled))
  "Alist of hydra build status numbers and status names.
Status numbers are returned by Hydra API, names (symbols) are
used internally by the elisp code of this package.")

(defun guix-hydra-build-status-number->name (number)
  "Convert build status number to a name.
See `guix-hydra-build-status-alist'."
  (bui-assq-value guix-hydra-build-status-alist number))

(defun guix-hydra-build-status-string (status)
  "Return a human readable string for build STATUS."
  (cl-case status
    (scheduled
     (bui-get-string "Scheduled" 'guix-hydra-build-status-scheduled))
    (running
     (bui-get-string "Running" 'guix-hydra-build-status-running))
    (succeeded
     (bui-get-string "Succeeded" 'guix-hydra-build-status-succeeded))
    (cancelled
     (bui-get-string "Cancelled" 'guix-hydra-build-status-cancelled))
    (failed-build
     (guix-hydra-build-status-fail-string))
    (failed-dependency
     (guix-hydra-build-status-fail-string "dependency"))
    (failed-other
     (guix-hydra-build-status-fail-string "other"))))

(defun guix-hydra-build-status-fail-string (&optional reason)
  "Return a string for a failed build."
  (let ((base (bui-get-string "Failed" 'guix-hydra-build-status-failed)))
    (if reason
        (concat base " (" reason ")")
      base)))

(defun guix-hydra-build-finished? (entry)
  "Return non-nil, if hydra build was finished."
  (bui-entry-non-void-value entry 'finished))

(defun guix-hydra-build-running? (entry)
  "Return non-nil, if hydra build is running."
  (eq (bui-entry-non-void-value entry 'status)
      'running))

(defun guix-hydra-build-scheduled? (entry)
  "Return non-nil, if hydra build is scheduled."
  (eq (bui-entry-non-void-value entry 'status)
      'scheduled))

(defun guix-hydra-build-succeeded? (entry)
  "Return non-nil, if hydra build succeeded."
  (eq (bui-entry-non-void-value entry 'status)
      'succeeded))

(defun guix-hydra-build-cancelled? (entry)
  "Return non-nil, if hydra build was cancelled."
  (eq (bui-entry-non-void-value entry 'status)
      'cancelled))

(defun guix-hydra-build-failed? (entry)
  "Return non-nil, if hydra build failed."
  (memq (bui-entry-non-void-value entry 'status)
        '(failed-build failed-dependency failed-other)))


;;; Hydra build 'info'

(guix-hydra-define-interface build info
  :mode-name "Hydra-Build-Info"
  :buffer-name "*Guix Hydra Build Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            guix-hydra-build-info-insert-url
            (time     format (time))
            (status   format guix-hydra-build-info-insert-status)
            (project  format (format guix-hydra-build-project))
            (jobset   format (format guix-hydra-build-jobset))
            (job      format (format guix-hydra-build-job))
            (system   format (format guix-hydra-build-system))
            (priority format (format))))

(defface guix-hydra-build-info-project
  '((t :inherit link))
  "Face for project names."
  :group 'guix-hydra-build-info-faces)

(defface guix-hydra-build-info-jobset
  '((t :inherit link))
  "Face for jobsets."
  :group 'guix-hydra-build-info-faces)

(defface guix-hydra-build-info-job
  '((t :inherit link))
  "Face for jobs."
  :group 'guix-hydra-build-info-faces)

(defface guix-hydra-build-info-system
  '((t :inherit link))
  "Face for system names."
  :group 'guix-hydra-build-info-faces)

(defmacro guix-hydra-build-define-button (name)
  "Define `guix-hydra-build-NAME' button."
  (let* ((name-str    (symbol-name name))
         (button-name (intern (concat "guix-hydra-build-" name-str)))
         (face-name   (intern (concat "guix-hydra-build-info-" name-str)))
         (keyword     (intern (concat ":" name-str))))
    `(define-button-type ',button-name
       :supertype 'bui
       'face ',face-name
       'help-echo ,(format "\
Show latest builds for this %s (with prefix, prompt for all parameters)"
                           name-str)
       'action (lambda (btn)
                 (let ((args (guix-hydra-build-latest-prompt-args
                              ,keyword (button-label btn))))
                   (apply #'guix-hydra-build-get-display
                          'latest args))))))

(guix-hydra-build-define-button project)
(guix-hydra-build-define-button jobset)
(guix-hydra-build-define-button job)
(guix-hydra-build-define-button system)

(defun guix-hydra-build-info-insert-url (entry)
  "Insert Hydra URL for the build ENTRY."
  (bui-insert-button (guix-hydra-build-url (bui-entry-id entry))
                     'bui-url)
  (when (guix-hydra-build-finished? entry)
    (bui-insert-indent)
    (bui-insert-action-button
     "Build log"
     (lambda (btn)
       (guix-hydra-build-view-log (button-get btn 'id)))
     "View build log"
     'id (bui-entry-id entry)))
  (bui-newline))

(defun guix-hydra-build-info-insert-status (status &optional _)
  "Insert a string with build STATUS."
  (insert (guix-hydra-build-status-string status)))


;;; Hydra build 'list'

(guix-hydra-define-interface build list
  :describe-function 'guix-hydra-list-describe
  :mode-name "Hydra-Build-List"
  :buffer-name "*Guix Hydra Builds*"
  :format '((name nil 30 t)
            (system nil 16 t)
            (status guix-hydra-build-list-get-status 20 t)
            (project nil 10 t)
            (jobset nil 17 t)
            (time bui-list-get-time 20 t))
  :hint 'guix-hydra-build-list-hint)

(let ((map guix-hydra-build-list-mode-map))
  (define-key map (kbd "B") 'guix-hydra-build-list-latest-builds)
  (define-key map (kbd "L") 'guix-hydra-build-list-view-log))

(defvar guix-hydra-build-list-default-hint
  '(("\\[guix-hydra-build-list-latest-builds]")
    " show latest builds of the current job;\n"
    ("\\[guix-hydra-build-list-view-log]") " show build log;\n"))

(defun guix-hydra-build-list-hint ()
  (bui-format-hints
   guix-hydra-build-list-default-hint
   (bui-default-hint)))

(defun guix-hydra-build-list-get-status (status &optional _)
  "Return a string for build STATUS."
  (guix-hydra-build-status-string status))

(defun guix-hydra-build-list-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds of the current job.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (guix-hydra-build-latest-prompt-args
      :project (bui-entry-non-void-value entry 'project)
      :jobset  (bui-entry-non-void-value entry 'name)
      :job     (bui-entry-non-void-value entry 'job)
      :system  (bui-entry-non-void-value entry 'system))))
  (apply #'guix-hydra-latest-builds number args))

(defun guix-hydra-build-list-view-log ()
  "View build log of the current Hydra build."
  (interactive)
  (guix-hydra-build-view-log (bui-list-current-id)))


;;; Interactive commands

;;;###autoload
(defun guix-hydra-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds.
ARGS are the same arguments as for `guix-hydra-build-latest-api-url'.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive (guix-hydra-build-latest-prompt-args))
  (apply #'guix-hydra-build-get-display
         'latest number args))

;;;###autoload
(defun guix-hydra-queued-builds (number)
  "Display the NUMBER of queued Hydra builds."
  (interactive "NNumber of queued builds: ")
  (guix-hydra-build-get-display 'queue number))

(provide 'guix-hydra-build)

;;; guix-hydra-build.el ends here
