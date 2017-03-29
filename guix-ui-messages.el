;;; guix-ui-messages.el --- Minibuffer messages for Guix package management interface

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

;; This file provides `guix-result-message' function used to show a
;; minibuffer message after displaying packages/generations in a
;; list/info buffer.

;;; Code:

(require 'cl-lib)
(require 'bui-utils)

(defvar guix-messages
  `((package
     (id
      ,(lambda (_ entries ids)
         (guix-message-packages-by-id entries 'package ids)))
     (name
      ,(lambda (_ entries names)
         (guix-message-packages-by-name entries 'package names)))
     (license
      ,(lambda (_ entries licenses)
         (apply #'guix-message-packages-by-license
                entries 'package licenses)))
     (location
      ,(lambda (_ entries locations)
         (apply #'guix-message-packages-by-location
                entries 'package locations)))
     (from-file
      (0 "No package in file '%s'." val)
      (1 "Package from file '%s'." val))
     (from-os-file
      (0 "No packages in OS file '%s'." val)
      (1 "Package from OS file '%s'." val)
      (many "%d packages from OS file '%s'." count val))
     (regexp
      (0 "No packages matching '%s'." val)
      (1 "A single package matching '%s'." val)
      (many "%d packages matching '%s'." count val))
     (all-available
      (0 "No packages are available for some reason.")
      (1 "A single available package (that's strange).")
      (many "%d available packages." count))
     (newest-available
      (0 "No packages are available for some reason.")
      (1 "A single newest available package (that's strange).")
      (many "%d newest available packages." count))
     (installed
      (0 "No packages installed in profile '%s'." profile)
      (1 "A single package installed in profile '%s'." profile)
      (many "%d packages installed in profile '%s'." count profile))
     (obsolete
      (0 "No obsolete packages in profile '%s'." profile)
      (1 "A single obsolete package in profile '%s'." profile)
      (many "%d obsolete packages in profile '%s'." count profile)))

    (output
     (id
      ,(lambda (_ entries ids)
         (guix-message-packages-by-id entries 'output ids)))
     (name
      ,(lambda (_ entries names)
         (guix-message-packages-by-name entries 'output names)))
     (license
      ,(lambda (_ entries licenses)
         (apply #'guix-message-packages-by-license
                entries 'output licenses)))
     (location
      ,(lambda (_ entries locations)
         (apply #'guix-message-packages-by-location
                entries 'output locations)))
     (from-file
      (0 "No package in file '%s'." val)
      (1 "Package from file '%s'." val)
      (many "Package outputs from file '%s'." val))
     (from-os-file
      (0 "No packages in OS file '%s'." val)
      (1 "Package from OS file '%s'." val)
      (many "%d package outputs from OS file '%s'." count val))
     (regexp
      (0 "No package outputs matching '%s'." val)
      (1 "A single package output matching '%s'." val)
      (many "%d package outputs matching '%s'." count val))
     (all-available
      (0 "No package outputs are available for some reason.")
      (1 "A single available package output (that's strange).")
      (many "%d available package outputs." count))
     (newest-available
      (0 "No package outputs are available for some reason.")
      (1 "A single newest available package output (that's strange).")
      (many "%d newest available package outputs." count))
     (installed
      (0 "No package outputs installed in profile '%s'." profile)
      (1 "A single package output installed in profile '%s'." profile)
      (many "%d package outputs installed in profile '%s'." count profile))
     (obsolete
      (0 "No obsolete package outputs in profile '%s'." profile)
      (1 "A single obsolete package output in profile '%s'." profile)
      (many "%d obsolete package outputs in profile '%s'." count profile))
     (profile-diff
      guix-message-outputs-by-diff))

    (generation
     (id
      (0 "Generations not found.")
      (1 "")
      (many "%d generations." count))
     (last
      (0 "No generations in profile '%s'." profile)
      (1 "The last generation of profile '%s'." profile)
      (many "%d last generations of profile '%s'." count profile))
     (all
      (0 "No generations in profile '%s'." profile)
      (1 "A single generation available in profile '%s'." profile)
      (many "%d generations available in profile '%s'." count profile))
     (time
      guix-message-generations-by-time))))

(defun guix-message-string-name (name)
  "Return a quoted name string."
  (concat "'" name "'"))

(defun guix-message-string-entry-type (entry-type &optional plural)
  "Return a string denoting an ENTRY-TYPE."
  (cl-ecase entry-type
    (package
     (if plural "packages" "package"))
    (output
     (if plural "package outputs" "package output"))
    (generation
     (if plural "generations" "generation"))))

(defun guix-message-string-entries (count entry-type)
  "Return a string denoting the COUNT of ENTRY-TYPE entries."
  (cl-case count
    (0 (concat "No "
               (guix-message-string-entry-type
                entry-type 'plural)))
    (1 (concat "A single "
               (guix-message-string-entry-type
                entry-type)))
    (t (format "%d %s"
               count
               (guix-message-string-entry-type
                entry-type 'plural)))))

(defun guix-message-packages-by-id (entries entry-type ids)
  "Display a message for packages or outputs searched by IDS."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count entry-type))
         (str-end (if (> count 1)
                      (concat "with the following IDs: "
                              (mapconcat #'bui-get-string ids ", "))
                    (concat "with ID " (bui-get-string (car ids))))))
    (if (zerop count)
        (message (substitute-command-keys "%s %s.
Most likely, Guix REPL was restarted, so IDs are not actual
anymore, because they live only during the REPL process.

Or it may be some package variant that cannot be handled by
Emacs-Guix.  For example, it may be so called 'canonical package'
used by '%%base-packages' in an operating-system declaration.

Try \"\\[guix-search-by-name]\" to find this package.")
                 str-beg str-end)
      (message "%s %s." str-beg str-end))))

(defun guix-message-packages-by-name (entries entry-type names)
  "Display a message for packages or outputs searched by NAMES."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count entry-type))
         (str-end (if (cdr names)
                      (concat "matching the following names: "
                              (mapconcat #'guix-message-string-name
                                         names ", "))
                    (concat "with name "
                            (guix-message-string-name (car names))))))
    (message "%s %s." str-beg str-end)))

(defun guix-message-packages-by-license (entries entry-type license)
  "Display a message for packages or outputs searched by LICENSE."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count entry-type))
         (str-end (format "with license '%s'" license)))
    (message "%s %s." str-beg str-end)))

(defun guix-message-packages-by-location (entries entry-type location)
  "Display a message for packages or outputs searched by LOCATION."
  (let* ((count   (length entries))
         (str-beg (guix-message-string-entries count entry-type))
         (str-end (format "placed in '%s'" location)))
    (message "%s %s." str-beg str-end)))

(defun guix-message-generations-by-time (profile entries times)
  "Display a message for generations searched by TIMES."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count 'generation))
         (time-beg (bui-get-time-string (car  times)))
         (time-end (bui-get-time-string (cadr times))))
    (message (concat "%s of profile '%s'\n"
                     "matching time period '%s' - '%s'.")
             str-beg profile time-beg time-end)))

(defun guix-message-outputs-by-diff (_ entries profiles)
  "Display a message for outputs searched by PROFILES difference."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count 'output))
         (profile1 (car  profiles))
         (profile2 (cadr profiles)))
    (cl-multiple-value-bind (new old str-action)
        (if (string-lessp profile2 profile1)
            (list profile1 profile2 "added to")
          (list profile2 profile1 "removed from"))
      (message "%s %s profile '%s' comparing with profile '%s'."
               str-beg str-action new old))))

(defun guix-result-message (profile entries entry-type
                            search-type search-vals)
  "Display an appropriate message after displaying ENTRIES."
  (let* ((type-spec (bui-assq-value guix-messages
                                     (if (eq entry-type 'system-generation)
                                         'generation
                                       entry-type)
                                     search-type))
         (fun-or-count-spec (car type-spec)))
    (if (functionp fun-or-count-spec)
        (funcall fun-or-count-spec profile entries search-vals)
      (let* ((count     (length entries))
             (count-key (if (> count 1) 'many count))
             (msg-spec  (bui-assq-value type-spec count-key))
             (msg       (car msg-spec))
             (args      (cdr msg-spec)))
        (mapc (lambda (subst)
                (setq args (cl-substitute (cdr subst) (car subst) args)))
              `((count   . ,count)
                (val     . ,(car search-vals))
                (profile . ,profile)))
        (apply #'message msg args)))))

(provide 'guix-ui-messages)

;;; guix-ui-messages.el ends here
