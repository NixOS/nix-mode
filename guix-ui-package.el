;;; guix-ui-package.el --- Interface for displaying packages  -*- lexical-binding: t -*-

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

;; This file provides an interface for displaying packages and outputs
;; in 'list' and 'info' buffers, and commands for working with them.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'bui)
(require 'guix nil t)
(require 'guix-ui)
(require 'guix-misc)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-utils)
(require 'guix-hydra)
(require 'guix-hydra-build)
(require 'guix-read)
(require 'guix-license)
(require 'guix-location)
(require 'guix-package)
(require 'guix-profiles)

(guix-ui-define-entry-type package)
(guix-ui-define-entry-type output)

(defcustom guix-package-list-type 'output
  "Define how to display packages in 'list' buffer.
Should be a symbol `package' or `output' (if `output', display each
output on a separate line; if `package', display each package on
a separate line)."
  :type '(choice (const :tag "List of packages" package)
                 (const :tag "List of outputs" output))
  :group 'guix-package)

(defun guix-package-list-type ()
  "Return BUI list entry-type by `guix-package-list-type' variable."
  (guix-make-symbol guix-package-list-type))

;; To avoid compilation warning: this variable is actually defined later
;; along with the rest "package-list" interface stuff.
(defvar guix-package-list-show-single)

(defun guix-package-get-display (profile search-type &rest search-values)
  "Search for packages/outputs and show results.

If PROFILE is nil, use `guix-current-profile'.

See `guix-ui-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALUES.

Results are displayed in the list buffer, unless a single package
is found and `guix-package-list-show-single' is nil."
  (let* ((args    (cl-list* (or profile guix-current-profile)
                            search-type search-values))
         (entries (bui-get-entries (guix-package-list-type) 'list args)))
    (if (or guix-package-list-show-single
            (null entries)
            (cdr entries))
        (bui-display-entries entries (guix-package-list-type) 'list args)
      (bui-get-display-entries 'guix-package 'info args))))

(defun guix-package-entry->name-specification (entry &optional output)
  "Return name specification of the package ENTRY and OUTPUT."
  (guix-package-name-specification
   (bui-entry-non-void-value entry 'name)
   (bui-entry-non-void-value entry 'version)
   (or output (bui-entry-non-void-value entry 'output))))

(defun guix-package-entries->name-specifications (entries)
  "Return name specifications by the package or output ENTRIES."
  (cl-remove-duplicates (mapcar #'guix-package-entry->name-specification
                                entries)
                        :test #'string=))

(defun guix-package-entry-installed-outputs (entry)
  "Return a list of installed outputs for the package ENTRY."
  (--map (bui-entry-non-void-value it 'output)
         (bui-entry-non-void-value entry 'installed)))


;;; Processing package actions

(defvar guix-package-name-width 40
  "Width of a package name \"column\".
This variable is used in a buffer to confirm operation.")

(defun guix-process-package-actions (profile actions
                                     &optional operation-buffer)
  "Process package ACTIONS on PROFILE.
Each action is a list of the form:

  (ACTION-TYPE PACKAGE-SPEC ...)

ACTION-TYPE is one of the following symbols: `install',
`upgrade', `remove'/`delete'.
PACKAGE-SPEC should have the following form: (ID [OUTPUT] ...)."
  (let (install upgrade remove)
    (mapc (lambda (action)
            (let ((action-type (car action))
                  (specs (cdr action)))
              (cl-case action-type
                (install (setq install (append install specs)))
                (upgrade (setq upgrade (append upgrade specs)))
                ((remove delete) (setq remove (append remove specs))))))
          actions)
    (when (guix-continue-package-operation-p
           profile
           :install install :upgrade upgrade :remove remove)
      (guix-eval-in-repl
       (guix-make-guile-expression
        'process-package-actions profile
        :install install :upgrade upgrade :remove remove
        :use-substitutes? (or guix-use-substitutes 'f)
        :dry-run? (or guix-dry-run 'f))
       (and (not guix-dry-run) operation-buffer)))))

(cl-defun guix-continue-package-operation-p (profile
                                             &key install upgrade remove)
  "Return non-nil if a package operation should be continued.
Ask a user if needed (see `guix-operation-confirm').
INSTALL, UPGRADE, REMOVE are 'package action specifications'.
See `guix-process-package-actions' for details."
  (or (null guix-operation-confirm)
      (let* ((entries (guix-ui-get-entries
                       profile 'package 'id
                       (append (mapcar #'car install)
                               (mapcar #'car upgrade)
                               (mapcar #'car remove))
                       '(id name version location)))
             (install-strings (guix-get-package-strings install entries))
             (upgrade-strings (guix-get-package-strings upgrade entries))
             (remove-strings  (guix-get-package-strings remove entries)))
        (if (or install-strings upgrade-strings remove-strings)
            (let ((buf (get-buffer-create guix-temp-buffer-name)))
              (with-current-buffer buf
                (setq-local cursor-type nil)
                (setq buffer-read-only nil)
                (erase-buffer)
                (insert (propertize "Profile" 'face 'bold)
                        ": " profile "\n\n")
                (guix-insert-package-strings install-strings "install")
                (guix-insert-package-strings upgrade-strings "upgrade")
                (guix-insert-package-strings remove-strings "remove")
                (let ((win (temp-buffer-window-show
                            buf
                            '((display-buffer-reuse-window
                               display-buffer-at-bottom)
                              (window-height . fit-window-to-buffer)))))
                  (prog1 (guix-operation-prompt)
                    (quit-window nil win)))))
          (message "Nothing to be done.
If Guix REPL was restarted, the data is not up-to-date.")
          nil))))

(defun guix-get-package-strings (specs entries)
  "Return short package descriptions for performing package actions.
See `guix-process-package-actions' for the meaning of SPECS.
ENTRIES is a list of package entries to get info about packages."
  (-non-nil
   (-map (-lambda ((id . outputs))
           (--when-let (bui-entry-by-id entries id)
             (let ((location (bui-entry-non-void-value it 'location))
                   (name (guix-package-entry->name-specification it)))
               (with-temp-buffer
                 (insert name)
                 (when outputs
                   (insert ":" (guix-concat-strings outputs ",")))
                 (when location
                   (indent-to guix-package-name-width 2)
                   (insert "(" location ")"))
                 (buffer-string)))))
         specs)))

(defun guix-insert-package-strings (strings action)
  "Insert information STRINGS at point for performing package ACTION."
  (when strings
    (insert "Package(s) to " (propertize action 'face 'bold) ":\n")
    (dolist (str strings)
      (insert "  " str "\n"))
    (bui-newline)))


;;; Package 'info'

(guix-ui-define-interface package info
  :mode-name "Package-Info"
  :buffer-name "*Guix Package Info*"
  :get-entries-function 'guix-package-info-get-entries
  :format '(guix-package-info-insert-heading
            nil
            (synopsis nil (simple guix-package-info-synopsis))
            nil
            (description nil (simple guix-package-info-description))
            nil
            (outputs simple guix-package-info-insert-outputs)
            guix-package-info-insert-misc
            (source simple guix-package-info-insert-source)
            (location simple guix-package-info-insert-location)
            (home-url format (format bui-url))
            (license format (format guix-package-license))
            (systems format guix-package-info-insert-systems)
            (inputs format (format guix-package-input))
            (native-inputs format (format guix-package-native-input))
            (propagated-inputs format
                               (format guix-package-propagated-input)))
  :titles '((home-url . "Home page")
            (systems . "Supported systems"))
  :required '(id name version installed non-unique))

(bui-define-interface guix-installed-output info
  :format '((file-name simple guix-installed-output-info-insert-file-name)
            (dependencies simple (indent bui-file)))
  :titles '((file-name . "Store directory"))
  :reduced? t)

(defface guix-package-info-heading
  '((t :inherit bui-info-heading))
  "Face for package name and version headings."
  :group 'guix-package-info-faces)

(defface guix-package-info-name-button
  '((t :inherit button))
  "Face used for a full name that can be used to describe a package."
  :group 'guix-package-info-faces)

;; Currently, `guix-package-info-name' and `guix-package-info-version'
;; faces are not used, but they were in the past and may be used in
;; future, so they were not removed.

(defface guix-package-info-name
  '((t :inherit font-lock-keyword-face))
  "Face used for a name of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-version
  '((t :inherit font-lock-builtin-face))
  "Face used for a version of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-synopsis
  '((((type tty pc) (class color)) :weight bold)
    (t :height 1.1 :weight bold :inherit variable-pitch))
  "Face used for a synopsis of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-description
  '((t))
  "Face used for a description of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-license
  '((t :inherit font-lock-string-face))
  "Face used for a license of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-location
  '((t :inherit bui-file-name))
  "Face used for a location of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-source
  '((t :inherit bui-url :underline nil))
  "Face used for a source URL of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-installed-outputs
  '((default :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))
     :foreground "PaleGreen")
    (((class color) (min-colors 8))
     :foreground "green")
    (t :underline t))
  "Face used for installed outputs of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-uninstalled-outputs
  '((t :weight bold))
  "Face used for uninstalled outputs of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-obsolete
  '((t :inherit error))
  "Face used if a package is obsolete."
  :group 'guix-package-info-faces)

(defcustom guix-package-info-auto-find-package t
  "If non-nil, open store directory after pressing \"Show\" package button.
If nil, just display the store directory (or directories) without finding."
  :type 'boolean
  :group 'guix-package-info)

(defcustom guix-package-info-auto-find-source nil
  "If non-nil, open source file after pressing \"Show\" source button.
If nil, just display the source file name without finding."
  :type 'boolean
  :group 'guix-package-info)

(defcustom guix-package-info-auto-download-source t
  "If nil, do not automatically download a source file if it doesn't exist.
After pressing a \"Show\" button, a derivation of the package
source is calculated and a store file name is displayed.  If this
variable is non-nil and the source file does not exist in the
store, it will be automatically downloaded (with a possible
prompt depending on `guix-operation-confirm' variable)."
  :type 'boolean
  :group 'guix-package-info)

(defcustom guix-package-info-button-functions
  '(guix-package-info-insert-build-button
    guix-package-info-insert-build-log-button
    guix-package-info-insert-graph-button
    guix-package-info-insert-size-button
    guix-package-info-insert-lint-button)
  "List of functions used to insert package buttons in Info buffer.
Each function is called with 2 arguments: package ID and full name."
  :type '(repeat function)
  :group 'guix-package-info)

(defvar guix-package-info-download-buffer nil
  "Buffer from which a current download operation was performed.")

(defvar guix-package-info-output-format "%-10s"
  "String used to format output names of the packages.
It should be a '%s'-sequence.  After inserting an output name
formatted with this string, an action button is inserted.")

(defvar guix-package-info-obsolete-string "(This package is obsolete)"
  "String used if a package is obsolete.")

(define-button-type 'guix-package-location
  :supertype 'bui
  'face 'guix-package-info-location
  'help-echo "Find location of this package"
  'action (lambda (btn)
            (guix-find-location (button-label btn))))

(define-button-type 'guix-package-license
  :supertype 'bui
  'face 'guix-package-info-license
  'help-echo "Display license info"
  'action (lambda (btn)
            (require 'guix-ui-license)
            (bui-get-display-entries
             'guix-license 'info
             (list 'name (button-label btn))
             'add)))

(define-button-type 'guix-package-name
  :supertype 'bui
  'face 'guix-package-info-name-button
  'help-echo "Describe this package"
  'action (lambda (btn)
            (bui-get-display-entries-current
             'guix-package 'info
             (list (guix-ui-current-profile)
                   'name (or (button-get btn 'spec)
                             (button-label btn)))
             'add)))

(define-button-type 'guix-package-heading
  :supertype 'guix-package-name
  'face 'guix-package-info-heading)

(define-button-type 'guix-package-source
  :supertype 'bui
  'face 'guix-package-info-source
  'help-echo ""
  'action (lambda (_)
            ;; As a source may not be a real URL (e.g., "mirror://..."),
            ;; no action is bound to a source button.
            (message "Yes, this is the source URL. What did you expect?")))

(defun guix-package-info-get-entries (profile search-type
                                              &rest search-values)
  "Return 'package' entries for displaying them in 'info' buffer."
  (guix-eval-read
   (guix-make-guile-expression
    'package/output-sexps
    profile 'package search-type search-values
    (cl-union guix-package-info-required-params
              (bui-info-displayed-params 'guix-package)))))

(defun guix-package-info-insert-heading (entry)
  "Insert package ENTRY heading (name and version) at point."
  (bui-insert-button
   (concat (bui-entry-non-void-value entry 'name) " "
           (bui-entry-non-void-value entry 'version))
   'guix-package-heading
   'spec (guix-package-entry->name-specification entry))
  (bui-newline))

(defun guix-package-info-insert-location (location &optional _)
  "Insert package LOCATION at point."
  (bui-insert-non-nil location
    (let ((location-file (car (split-string location ":"))))
      (bui-info-insert-value-indent location 'guix-package-location)
      ;; Do not show "Packages" button if a package 'from file' is displayed.
      (unless (eq (guix-ui-current-search-type) 'from-file)
        (bui-insert-indent)
        (bui-insert-action-button
         "Packages"
         (lambda (btn)
           (guix-package-get-display (guix-ui-current-profile)
                                     'location
                                     (button-get btn 'location)))
         (format "Display packages from location '%s'" location-file)
         'location location-file)))))

(defun guix-package-info-insert-systems (systems entry)
  "Insert supported package SYSTEMS at point."
  (bui-info-insert-value-format
   systems 'guix-hydra-build-system
   'action (lambda (btn)
             (let ((args (guix-hydra-build-latest-prompt-args
                          :job (button-get btn 'job-name)
                          :system (button-label btn))))
               (apply #'guix-hydra-build-get-display
                      'latest args)))
   'job-name (guix-hydra-job-name-specification
              (bui-entry-non-void-value entry 'name)
              (bui-entry-non-void-value entry 'version))))

(defmacro guix-package-info-define-insert-inputs (&optional type)
  "Define a face and a function for inserting package inputs.
TYPE is a type of inputs.
Function name is `guix-package-info-insert-TYPE-inputs'.
Face name is `guix-package-info-TYPE-inputs'."
  (let* ((type-str (symbol-name type))
         (type-name (and type (concat type-str "-")))
         (type-desc (and type (concat type-str " ")))
         (face (intern (concat "guix-package-info-" type-name "inputs")))
         (btn  (intern (concat "guix-package-" type-name "input"))))
    `(progn
       (defface ,face
         '((t :inherit guix-package-info-name-button))
         ,(concat "Face used for " type-desc "inputs of a package.")
         :group 'guix-package-info-faces)

       (define-button-type ',btn
         :supertype 'guix-package-name
         'face ',face))))

(guix-package-info-define-insert-inputs)
(guix-package-info-define-insert-inputs native)
(guix-package-info-define-insert-inputs propagated)

(defun guix-package-info-insert-outputs (outputs entry)
  "Insert OUTPUTS from package ENTRY at point."
  (and (bui-entry-non-void-value entry 'obsolete)
       (guix-package-info-insert-obsolete-text))
  (and (bui-entry-non-void-value entry 'non-unique)
       (bui-entry-non-void-value entry 'installed)
       (guix-package-info-insert-non-unique-text
        (guix-package-entry->name-specification entry)))
  (bui-newline)
  (dolist (output outputs)
    (guix-package-info-insert-output output entry)))

(defun guix-package-info-insert-obsolete-text ()
  "Insert a message about obsolete package at point."
  (bui-insert-indent)
  (bui-format-insert guix-package-info-obsolete-string
                     'guix-package-info-obsolete))

(defun guix-package-info-insert-non-unique-text (full-name)
  "Insert a message about non-unique package with FULL-NAME at point."
  (bui-newline)
  (bui-insert-indent)
  (insert "Installed outputs are displayed for a non-unique ")
  (bui-insert-button full-name 'guix-package-name)
  (insert " package."))

(defun guix-package-info-insert-output (output entry)
  "Insert OUTPUT at point.
Make some fancy text with buttons and additional stuff if the
current OUTPUT is installed (if there is such output in
`installed' parameter of a package ENTRY)."
  (let* ((installed (bui-entry-non-void-value entry 'installed))
         (obsolete  (bui-entry-non-void-value entry 'obsolete))
         (installed-entry (--find
                           (string= (bui-entry-non-void-value it 'output)
                                    output)
                           installed))
         (action-type (if installed-entry 'delete 'install))
         (profile (guix-ui-current-profile)))
    (bui-insert-indent)
    (bui-format-insert output
                       (if installed-entry
                           'guix-package-info-installed-outputs
                         'guix-package-info-uninstalled-outputs)
                       guix-package-info-output-format)
    ;; Do not allow a user to install/delete anything to/from a system
    ;; profile, so add action buttons only for non-system profiles.
    (when (and profile
               (not (guix-system-profile? profile)))
      (guix-package-info-insert-action-button action-type entry output)
      (when obsolete
        (bui-insert-indent)
        (guix-package-info-insert-action-button 'upgrade entry output)))
    (bui-newline)
    (when installed-entry
      (bui-info-insert-entry installed-entry 'guix-installed-output 2))))

(defun guix-installed-output-info-insert-file-name (file-name &optional _)
  "Insert package store FILE-NAME at point."
  (bui-insert-non-nil file-name
    (bui-info-insert-value-indent file-name 'bui-file)
    (bui-insert-indent)
    (bui-insert-action-button
     "Size"
     (lambda (btn)
       (guix-package-size (button-get btn 'file-name)
                          (guix-read-package-size-type)))
     "View package size of the current store file name"
     'file-name file-name)))

(defun guix-package-info-insert-action-button (type entry output)
  "Insert button to process an action on a package OUTPUT at point.
TYPE is one of the following symbols: `install', `delete', `upgrade'.
ENTRY is an alist with package info."
  (let ((type-str  (capitalize (symbol-name type)))
        (full-name (guix-package-entry->name-specification entry output)))
    (bui-insert-action-button
     type-str
     (lambda (btn)
       (guix-process-package-actions
        (guix-ui-current-profile)
        `((,(button-get btn 'action-type) (,(button-get btn 'id)
                                           ,(button-get btn 'output))))
        (current-buffer)))
     (concat type-str " '" full-name "'")
     'action-type type
     'id (or (bui-entry-non-void-value entry 'package-id)
             (bui-entry-id entry))
     'output output)))

(defun guix-package-info-show-store-path (entry-id package-id)
  "Show store directories of the package outputs in the current buffer.
ENTRY-ID is an ID of the current entry (package or output).
PACKAGE-ID is an ID of the package which store path to show."
  (let* ((entries (bui-current-entries))
         (entry   (bui-entry-by-id entries entry-id))
         (dirs    (guix-package-store-path package-id)))
    (or dirs
        (error "Couldn't define store directory of the package"))
    (let* ((new-entry (cons (cons 'store-path dirs)
                            entry))
           (new-entries (bui-replace-entry entries entry-id new-entry)))
      (setf (bui-item-entries bui-item)
            new-entries)
      (bui-redisplay-goto-button)
      (let ((dir (car dirs)))
        (if (file-exists-p dir)
            (if guix-package-info-auto-find-package
                (find-file dir)
              (message nil))
          (message "'%s' does not exist.\nTry to build this package."
                   dir))))))

(defun guix-package-info-insert-misc (entry)
  "Insert various buttons and other info for package ENTRY at point."
  (unless (bui-entry-non-void-value entry 'obsolete)
    (let* ((entry-id   (bui-entry-id entry))
           (package-id (or (bui-entry-non-void-value entry 'package-id)
                           entry-id))
           (full-name  (guix-package-entry->name-specification entry))
           (store-path (bui-entry-non-void-value entry 'store-path)))
      (bui-info-insert-title-simple "Package")
      (if store-path
          (bui-info-insert-value-indent store-path 'bui-file)
        (bui-insert-action-button
         "Show"
         (lambda (btn)
           (guix-package-info-show-store-path
            (button-get btn 'entry-id)
            (button-get btn 'package-id)))
         "Show the store directory of the current package"
         'entry-id entry-id
         'package-id package-id))
      (when guix-package-info-button-functions
        (bui-newline)
        (bui-mapinsert (lambda (fun)
                         (funcall fun package-id full-name))
                       guix-package-info-button-functions
                       (bui-get-indent)
                       :indent bui-indent
                       :column (bui-fill-column)))
      (bui-newline))))

(defun guix-package-info-insert-build-button (id full-name)
  "Insert button to build a package defined by ID."
  (bui-insert-action-button
   "Build"
   (lambda (btn)
     (guix-build-package (button-get btn 'id)
                         (format "Build '%s' package?" full-name)))
   (format "Build the current package")
   'id id))

(defun guix-package-info-insert-build-log-button (id _name)
  "Insert button to show build log of a package defined by ID."
  (bui-insert-action-button
   "Build Log"
   (lambda (btn)
     (guix-package-find-build-log (button-get btn 'id)))
   "View build log of the current package"
   'id id))

(declare-function guix-package-graph "guix-graph" t)

(defun guix-package-info-insert-graph-button (id _name)
  "Insert button to show a graph of a package defined by ID."
  (bui-insert-action-button
   "Graph"
   (lambda (btn)
     (guix-package-graph (button-get btn 'id)
                         (guix-read-graph-backend)
                         (guix-read-graph-node-type)))
   "View graph of the current package"
   'id id))

(defun guix-package-info-insert-size-button (_id name)
  "Insert button to show a size of a package NAME."
  (bui-insert-action-button
   "Size"
   (lambda (btn)
     (guix-package-size (button-get btn 'name)
                        (guix-read-package-size-type)))
   (format "View size of '%s' package" name)
   'name name))

(defun guix-package-info-insert-lint-button (id _name)
  "Insert button to lint a package defined by ID."
  (bui-insert-action-button
   "Lint"
   (lambda (btn)
     (guix-lint (button-get btn 'id)
                (and current-prefix-arg
                     (guix-read-lint-checker-names))))
   "Lint the current package"
   'id id))

(defun guix-package-info-show-source (entry-id package-id)
  "Show file name of a package source in the current info buffer.
Find the file if needed (see `guix-package-info-auto-find-source').
ENTRY-ID is an ID of the current entry (package or output).
PACKAGE-ID is an ID of the package which source to show."
  (let* ((entries (bui-current-entries))
         (entry   (bui-entry-by-id entries entry-id))
         (file    (guix-package-source-file-name package-id)))
    (or file
        (error "Couldn't define file name of the package source"))
    (let* ((new-entry (cons (cons 'source-file file)
                            entry))
           (new-entries (bui-replace-entry entries entry-id new-entry)))
      (setf (bui-item-entries bui-item)
            new-entries)
      (bui-redisplay-goto-button)
      (if (file-exists-p file)
          (if guix-package-info-auto-find-source
              (guix-find-file file)
            (message "The file name of the package source is displayed."))
        (if guix-package-info-auto-download-source
            (guix-package-info-download-source package-id)
          (message "The source does not exist in the store."))))))

(defun guix-package-info-download-source (package-id)
  "Download a source of the package PACKAGE-ID."
  (setq guix-package-info-download-buffer (current-buffer))
  (guix-package-source-build-derivation
   package-id
   "The source does not exist in the store. Download it?"))

(defun guix-package-info-insert-source (source entry)
  "Insert SOURCE from package ENTRY at point.
SOURCE is a list of URLs."
  (bui-insert-non-nil source
    (let* ((source-file (bui-entry-non-void-value entry 'source-file))
           (entry-id    (bui-entry-id entry))
           (package-id  (or (bui-entry-non-void-value entry 'package-id)
                            entry-id)))
      (if (null source-file)
          (bui-insert-action-button
           "Show"
           (lambda (btn)
             (guix-package-info-show-source (button-get btn 'entry-id)
                                            (button-get btn 'package-id)))
           "Show the source store directory of the current package"
           'entry-id entry-id
           'package-id package-id)
        (unless (file-exists-p source-file)
          (bui-insert-action-button
           "Download"
           (lambda (btn)
             (guix-package-info-download-source
              (button-get btn 'package-id)))
           "Download the source into the store"
           'package-id package-id))
        (bui-info-insert-value-indent source-file 'bui-file))
      (bui-info-insert-value-indent source 'guix-package-source))))

(defun guix-package-info-redisplay-after-download ()
  "Redisplay an 'info' buffer after downloading the package source.
This function is used to hide a \"Download\" button if needed."
  (when (buffer-live-p guix-package-info-download-buffer)
    (with-current-buffer guix-package-info-download-buffer
      (bui-redisplay-goto-button))
    (setq guix-package-info-download-buffer nil)))

(add-hook 'guix-after-source-download-hook
          'guix-package-info-redisplay-after-download)


;;; Package 'list'

(guix-ui-define-interface package list
  :mode-name "Package-List"
  :buffer-name "*Guix Packages*"
  :get-entries-function 'guix-package-list-get-entries
  :describe-function 'guix-ui-list-describe
  :format '((name guix-package-list-get-name 20 t)
            (version nil 10 nil)
            (outputs nil 13 t)
            (installed guix-package-list-get-installed-outputs 13 t)
            (synopsis bui-list-get-one-line 30 nil))
  :hint 'guix-package-list-hint
  :sort-key '(name)
  :marks '((install . ?I)
           (upgrade . ?U)
           (delete  . ?D)))

(let ((map guix-package-list-mode-map))
  (define-key map (kbd "B")   'guix-package-list-latest-builds)
  (define-key map (kbd "G")   'guix-package-list-graph)
  (define-key map (kbd "z")   'guix-package-list-size)
  (define-key map (kbd "L")   'guix-package-list-lint)
  (define-key map (kbd "e")   'guix-package-list-edit)
  (define-key map (kbd "x")   'guix-package-list-execute)
  (define-key map (kbd "i")   'guix-package-list-mark-install)
  (define-key map (kbd "d")   'guix-package-list-mark-delete)
  (define-key map (kbd "U")   'guix-package-list-mark-upgrade)
  (define-key map (kbd "^")   'guix-package-list-mark-upgrades))

(defface guix-package-list-installed
  '((t :inherit guix-package-info-installed-outputs))
  "Face used if there are installed outputs for the current package."
  :group 'guix-package-list-faces)

(defface guix-package-list-obsolete
  '((t :inherit guix-package-info-obsolete))
  "Face used if a package is obsolete."
  :group 'guix-package-list-faces)

(defcustom guix-package-list-generation-marking-enabled nil
  "If non-nil, allow putting marks in a list with 'generation packages'.

By default this is disabled, because it may be confusing.  For
example, a package is installed in some generation, so a user can
mark it for deletion in the list of packages from this
generation, but the package may not be installed in the latest
generation, so actually it cannot be deleted.

If you managed to understand the explanation above or if you
really know what you do or if you just don't care, you can set
this variable to t.  It should not do much harm anyway (most
likely)."
  :type 'boolean
  :group 'guix-package-list)

(defvar guix-package-list-default-hint
  '(("\\[guix-package-list-edit]") " edit (go to) the package definition;\n"
    ("\\[guix-package-list-graph]") " view package graph; "
    ("\\[guix-package-list-size]") " view package size; "
    ("\\[guix-package-list-lint]") " lint;\n"
    ("\\[guix-package-list-mark-install]") " mark for installation; "
    ("\\[guix-package-list-mark-delete]") " mark for deletion;\n"
    ("\\[guix-package-list-mark-upgrade]") " mark for upgrading; "
    ("\\[guix-package-list-mark-upgrades]") " mark all for upgrading;\n"
    ("\\[guix-package-list-execute]") " execute operation;\n"))

(defun guix-package-list-hint ()
  (bui-format-hints
   guix-package-list-default-hint
   (bui-default-hint)))

(defun guix-package-list-get-entries (profile search-type
                                              &rest search-values)
  "Return 'package' entries for displaying them in 'list' buffer."
  (guix-eval-read
   (guix-make-guile-expression
    'package/output-sexps
    profile 'package search-type search-values
    (cl-union guix-package-list-required-params
              (bui-list-displayed-params 'guix-package)))))

(defun guix-package-list-get-name (name entry)
  "Return NAME of the package ENTRY.
Colorize it with `guix-package-list-installed' or
`guix-package-list-obsolete' if needed."
  (bui-get-string
   name
   (cond ((bui-entry-non-void-value entry 'obsolete)
          'guix-package-list-obsolete)
         ((bui-entry-non-void-value entry 'installed)
          'guix-package-list-installed))))

(defun guix-package-list-get-installed-outputs (installed &optional _)
  "Return string with outputs from INSTALLED entries."
  (bui-get-string
   (--map (bui-entry-non-void-value it 'output)
          installed)))

(defun guix-package-list-marking-check ()
  "Signal an error if marking is disabled for the current buffer."
  (when (and (not guix-package-list-generation-marking-enabled)
             (or (derived-mode-p 'guix-package-list-mode)
                 (derived-mode-p 'guix-output-list-mode))
             (eq (guix-ui-current-search-type) 'generation))
    (error "Action marks are disabled for lists of 'generation packages'")))

(defun guix-package-list-mark-outputs (mark default
                                       &optional prompt available)
  "Mark the current package with MARK and move to the next line.
If PROMPT is non-nil, use it to ask a user for outputs from
AVAILABLE list, otherwise mark all DEFAULT outputs."
  (let ((outputs (if prompt
                     (guix-completing-read-multiple
                      prompt available nil t)
                   default)))
    (apply #'bui-list--mark mark t outputs)))

(defun guix-package-list-mark-install (&optional arg)
  "Mark the current package for installation and move to the next line.
With ARG, prompt for the outputs to install (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry     (bui-list-current-entry))
         (all       (bui-entry-non-void-value entry 'outputs))
         (installed (guix-package-entry-installed-outputs entry))
         (available (cl-set-difference all installed :test #'string=)))
    (or available
        (user-error "This package is already installed"))
    (guix-package-list-mark-outputs
     'install '("out")
     (and arg "Output(s) to install: ")
     available)))

(defun guix-package-list-mark-delete (&optional arg)
  "Mark the current package for deletion and move to the next line.
With ARG, prompt for the outputs to delete (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry (bui-list-current-entry))
         (installed (guix-package-entry-installed-outputs entry)))
    (or installed
        (user-error "This package is not installed"))
    (guix-package-list-mark-outputs
     'delete installed
     (and arg "Output(s) to delete: ")
     installed)))

(defun guix-package-list-mark-upgrade (&optional arg)
  "Mark the current package for upgrading and move to the next line.
With ARG, prompt for the outputs to upgrade (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry (bui-list-current-entry))
         (installed (guix-package-entry-installed-outputs entry)))
    (or installed
        (user-error "This package is not installed"))
    (when (or (bui-entry-non-void-value entry 'obsolete)
              (y-or-n-p "This package is not obsolete.  Try to upgrade it anyway? "))
      (guix-package-list-mark-outputs
       'upgrade installed
       (and arg "Output(s) to upgrade: ")
       installed))))

(defun guix-package-mark-upgrades (fun &optional all)
  "Mark all obsolete packages for upgrading.
Use FUN to perform marking of the current line.  FUN should
take an entry as argument.
If ALL is non-nil, mark all installed (not only obsolete) packages."
  (guix-package-list-marking-check)
  (let ((entries (--filter (bui-entry-non-void-value
                            it (if all 'installed 'obsolete))
                           (bui-current-entries))))
    (bui-list-for-each-line
     (lambda ()
       (let* ((id (bui-list-current-id))
              (entry (--find (equal id (bui-entry-id it))
                             entries)))
         (when entry
           (funcall fun entry)))))))

(defun guix-package-list-mark-upgrades (&optional arg)
  "Mark all obsolete packages for upgrading.
With ARG, mark all installed (including non-obsolete) packages."
  (interactive "P")
  (guix-package-mark-upgrades
   (lambda (entry)
     (apply #'bui-list--mark
            'upgrade nil
            (guix-package-entry-installed-outputs entry)))
   arg))

(defun guix-package-assert-non-system-profile ()
  "Verify that the current profile is not a system one.
The current profile is the one used by the current buffer."
  (--when-let (guix-ui-current-profile)
    (guix-assert-non-system-profile it)))

(defun guix-package-execute-actions (fun)
  "Perform actions on the marked packages.
Use FUN to define actions suitable for `guix-process-package-actions'.
FUN should take action-type as argument."
  (guix-package-assert-non-system-profile)
  (let ((actions (-non-nil (mapcar fun '(install delete upgrade)))))
    (if actions
        (guix-process-package-actions (guix-ui-current-profile)
                                      actions (current-buffer))
      (user-error "No operations specified"))))

(defun guix-package-list-execute ()
  "Perform actions on the marked packages."
  (interactive)
  (guix-package-execute-actions #'guix-package-list-make-action))

(defun guix-package-list-make-action (action-type)
  "Return action specification for the packages marked with ACTION-TYPE.
Return nil, if there are no packages marked with ACTION-TYPE.
The specification is suitable for `guix-process-package-actions'."
  (let ((specs (bui-list-get-marked-args action-type)))
    (and specs (cons action-type specs))))

(defun guix-package-list-edit (&optional directory)
  "Go to the location of the current package.
See `guix-find-location' for the meaning of DIRECTORY."
  (interactive (list (guix-read-directory)))
  (guix-edit (bui-list-current-id) directory))

(defun guix-package-list-graph (backend node-type)
  "Show BACKEND/NODE-TYPE graph for the current package."
  (interactive
   (list (guix-read-graph-backend)
         (guix-read-graph-node-type)))
  (let ((entry (bui-list-current-entry)))
    (guix-package-graph (if (bui-entry-non-void-value entry 'obsolete)
                            (bui-entry-non-void-value entry 'name)
                          (bui-list-current-id))
                        backend node-type)))

(defun guix-package-list-size (&optional type)
  "Show size of the current package.
See `guix-package-size' for the meaning of TYPE."
  (interactive (list (guix-read-package-size-type)))
  (guix-package-size (guix-package-entry->name-specification
                      (bui-list-current-entry))
                     type))

(defun guix-package-list-lint (&optional checkers)
  "Lint the current package.
Interactively with prefix, prompt for CHECKERS.
See `guix-lint' for details."
  (interactive
   (list (and current-prefix-arg
              (guix-read-lint-checker-names))))
  (guix-lint (bui-list-current-id) checkers))

(defun guix-package-list-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds of the current package.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (guix-hydra-build-latest-prompt-args
      :job (guix-hydra-job-name-specification
            (bui-entry-non-void-value entry 'name)
            (bui-entry-non-void-value entry 'version)))))
  (apply #'guix-hydra-latest-builds number args))


;;; Output 'list'

(guix-ui-define-interface output list
  :mode-name "Output-List"
  :buffer-name "*Guix Packages*"
  :get-entries-function 'guix-output-list-get-entries
  :describe-function 'guix-output-list-describe
  :format '((name guix-package-list-get-name 20 t)
            (version nil 10 nil)
            (output nil 9 t)
            (installed nil 12 t)
            (synopsis bui-list-get-one-line 30 nil))
  :required '(id package-id)
  :hint 'guix-output-list-hint
  :sort-key '(name)
  :marks '((install . ?I)
           (upgrade . ?U)
           (delete  . ?D)))

(let ((map guix-output-list-mode-map))
  (define-key map (kbd "B")   'guix-package-list-latest-builds)
  (define-key map (kbd "G")   'guix-output-list-graph)
  (define-key map (kbd "z")   'guix-package-list-size)
  (define-key map (kbd "L")   'guix-output-list-lint)
  (define-key map (kbd "e")   'guix-output-list-edit)
  (define-key map (kbd "x")   'guix-output-list-execute)
  (define-key map (kbd "i")   'guix-output-list-mark-install)
  (define-key map (kbd "d")   'guix-output-list-mark-delete)
  (define-key map (kbd "U")   'guix-output-list-mark-upgrade)
  (define-key map (kbd "^")   'guix-output-list-mark-upgrades))

(defvar guix-output-list-default-hint
  '(("\\[guix-output-list-edit]") " edit (go to) the package definition;\n"
    ("\\[guix-output-list-graph]") " view package graph; "
    ("\\[guix-package-list-size]") " view package size; "
    ("\\[guix-output-list-lint]") " lint;\n"
    ("\\[guix-output-list-mark-install]") " mark for installation; "
    ("\\[guix-output-list-mark-delete]") " mark for deletion;\n"
    ("\\[guix-output-list-mark-upgrade]") " mark for upgrading; "
    ("\\[guix-output-list-mark-upgrades]") " mark all for upgrading;\n"
    ("\\[guix-output-list-execute]") " execute operation;\n"))

(defun guix-output-list-hint ()
  (bui-format-hints
   guix-output-list-default-hint
   (bui-default-hint)))

(defun guix-output-list-get-entries (profile search-type
                                             &rest search-values)
  "Return 'output' entries for displaying them in 'list' buffer."
  (guix-eval-read
   (guix-make-guile-expression
    'package/output-sexps
    profile 'output search-type search-values
    (cl-union guix-output-list-required-params
              (bui-list-displayed-params 'guix-output)))))

(defun guix-output-list-mark-install ()
  "Mark the current output for installation and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (bui-list-current-entry))
         (installed (bui-entry-non-void-value entry 'installed)))
    (if installed
        (user-error "This output is already installed")
      (bui-list--mark 'install t))))

(defun guix-output-list-mark-delete ()
  "Mark the current output for deletion and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (bui-list-current-entry))
         (installed (bui-entry-non-void-value entry 'installed)))
    (if installed
        (bui-list--mark 'delete t)
      (user-error "This output is not installed"))))

(defun guix-output-list-mark-upgrade ()
  "Mark the current output for upgrading and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (bui-list-current-entry))
         (installed (bui-entry-non-void-value entry 'installed)))
    (or installed
        (user-error "This output is not installed"))
    (when (or (bui-entry-non-void-value entry 'obsolete)
              (y-or-n-p "This output is not obsolete.  Try to upgrade it anyway? "))
      (bui-list--mark 'upgrade t))))

(defun guix-output-list-mark-upgrades (&optional arg)
  "Mark all obsolete package outputs for upgrading.
With ARG, mark all installed (including non-obsolete) packages."
  (interactive "P")
  (guix-package-mark-upgrades
   (lambda (_) (bui-list--mark 'upgrade))
   arg))

(defun guix-output-list-execute ()
  "Perform actions on the marked outputs."
  (interactive)
  (guix-package-execute-actions #'guix-output-list-make-action))

(defun guix-output-list-make-action (action-type)
  "Return action specification for the outputs marked with ACTION-TYPE.
Return nil, if there are no outputs marked with ACTION-TYPE.
The specification is suitable for `guix-process-output-actions'."
  (let ((ids (bui-list-get-marked-id-list action-type)))
    (and ids (cons action-type
                   (mapcar #'guix-package-id-and-output-by-output-id
                           ids)))))

(defun guix-output-list-describe (&rest ids)
  "Describe outputs with IDS (list of output identifiers)."
  (let ((pids (--map (car (guix-package-id-and-output-by-output-id it))
                     ids)))
    (bui-get-display-entries
     'guix-package 'info
     (cl-list* (guix-ui-current-profile)
               'id (cl-remove-duplicates pids))
     'add)))

(defun guix-output-list-edit (&optional directory)
  "Go to the location of the current package.
See `guix-find-location' for the meaning of DIRECTORY."
  (interactive (list (guix-read-directory)))
  (guix-edit (bui-entry-non-void-value (bui-list-current-entry)
                                       'package-id)
             directory))

(defun guix-output-list-graph (backend node-type)
  "Show BACKEND/NODE-TYPE graph for the current package."
  (interactive
   (list (guix-read-graph-backend)
         (guix-read-graph-node-type)))
  (let ((entry (bui-list-current-entry)))
    (guix-package-graph (if (bui-entry-non-void-value entry 'obsolete)
                            (bui-entry-non-void-value entry 'name)
                          (bui-entry-non-void-value entry 'package-id))
                        backend node-type)))

(defun guix-output-list-lint (&optional checkers)
  "Lint the current package.
Interactively with prefix, prompt for CHECKERS.
See `guix-lint' for details."
  (interactive
   (list (and current-prefix-arg
              (guix-read-lint-checker-names))))
  (guix-lint (bui-entry-non-void-value (bui-list-current-entry)
                                       'package-id)
             checkers))


;;; Interactive commands

(defvar guix-package-search-params '(name synopsis description)
  "Default list of package parameters for searching by regexp.")

(defvar guix-package-search-history nil
  "A history of minibuffer prompts.")

;;;###autoload
(defun guix-packages-by-name (name &optional profile)
  "Display Guix packages with NAME.
NAME is a string with name specification.  It may optionally contain
a version number.  Examples: \"guile\", \"guile@2.0.11\".

If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-package-name)
         (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'name name))

;;;###autoload
(defun guix-packages-by-license (license &optional profile)
  "Display Guix packages with LICENSE.
LICENSE is a license name string.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-license-name)
         (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'license license))

;;;###autoload
(defun guix-packages-by-location (location &optional profile)
  "Display Guix packages placed in LOCATION file.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-package-location)
         (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'location location))

;;;###autoload
(defun guix-package-from-file (file &optional profile)
  "Display Guix package that the code from FILE evaluates to.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-file-name "File with package: ")
         (guix-ui-read-package-profile)))
  (bui-get-display-entries
   'guix-package 'info
   (list (or profile guix-current-profile) 'from-file file)
   'add))

;;;###autoload
(defun guix-packages-from-system-config-file (file &optional profile)
  "Display Guix packages from the operating system configuration FILE.

Make sure FILE has a proper operating-system declaration.  You
may check it, for example, by running the following shell command:

  guix system build --dry-run FILE

See also Info node `(guix) System Configuration'.

If PROFILE is nil, use system profile (it is used to show what
packages from FILE are installed in PROFILE).

Interactively, prompt for FILE.  With prefix argument, also prompt
for PROFILE.

Note: This command displays only those packages that are placed
in 'packages' field of the 'operating-system' declaration.  An
installed system also contains packages installed by
services (like 'guix' or 'shepherd').  To see all the packages
installed in a system profile, use
'\\[guix-installed-system-packages]' command."
  (interactive
   (list (guix-read-file-name "System configuration file: ")
         (and current-prefix-arg
              (guix-read-package-profile guix-system-profile))))
  (guix-package-get-display (or profile (guix-package-profile
                                         guix-system-profile))
                            'from-os-file file))

;;;###autoload
(defun guix-search-by-regexp (regexp &optional params profile)
  "Search for Guix packages by REGEXP.
PARAMS are package parameters that should be searched.
If PARAMS are not specified, use `guix-package-search-params'.

If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-regexp "Regexp: " nil 'guix-package-search-history)
         nil (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'regexp regexp
                            (or params guix-package-search-params)))

;;;###autoload
(defun guix-search-by-name (regexp &optional profile)
  "Search for Guix packages matching REGEXP in a package name.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-string "Package name by regexp: "
                      nil 'guix-package-search-history)
         (guix-ui-read-package-profile)))
  (guix-search-by-regexp regexp '(name) profile))

;;;###autoload
(defun guix-installed-packages (&optional profile)
  "Display information about installed Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'installed))

;;;###autoload
(defun guix-installed-user-packages ()
  "Display information about Guix packages installed in a user profile."
  (interactive)
  (guix-installed-packages (guix-package-profile guix-user-profile)))

;;;###autoload
(defun guix-installed-system-packages ()
  "Display information about Guix packages installed in a system profile."
  (interactive)
  (guix-installed-packages (guix-package-profile guix-system-profile)))

;;;###autoload
(defun guix-obsolete-packages (&optional profile)
  "Display information about obsolete Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'obsolete))

;;;###autoload
(defun guix-all-available-packages (&optional profile)
  "Display information about all available Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'all-available))

;;;###autoload
(defun guix-newest-available-packages (&optional profile)
  "Display information about the newest available Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-package-profile)))
  (guix-package-get-display profile 'newest-available))

(provide 'guix-ui-package)

;;; guix-ui-package.el ends here
