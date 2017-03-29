;;; guix-ui-generation.el --- Interface for displaying generations  -*- lexical-binding: t -*-

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

;; This file provides an interface for displaying profile generations in
;; 'list' and 'info' buffers, and commands for working with them.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'bui)
(require 'guix nil t)
(require 'guix-ui)
(require 'guix-ui-package)
(require 'guix-misc)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-utils)
(require 'guix-profiles)

(guix-ui-define-entry-type generation)

(defun guix-generation-get-entries (proc profile search-type
                                         search-values params)
  "Return 'generation' or 'system-generation' entries.
PROC is the name of a Scheme procedure (either 'generation-sexps'
or 'system-generation-sexps')."
  (apply #'guix-modify-objects
         (guix-eval-read (guix-make-guile-expression
                          proc profile search-type search-values params))
         (when (or (null params)
                   (memq 'number-of-packages params))
           (list
            (lambda (entry)
              (let ((generation (bui-entry-non-void-value entry 'number)))
                (if generation
                    `((number-of-packages
                       . ,(guix-profile-number-of-packages
                           profile generation))
                      ,@entry)
                  entry)))))))

(defun guix-generation-get-display (profile search-type &rest search-values)
  "Search for generations and show results.

If PROFILE is nil, use `guix-current-profile'.

See `guix-ui-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALUES."
  (apply #'bui-list-get-display-entries
         'guix-generation
         (or profile guix-current-profile)
         search-type search-values))

(defun guix-delete-generations (profile generations
                                &optional operation-buffer)
  "Delete GENERATIONS from PROFILE.
Each element from GENERATIONS is a generation number."
  (when (or (not guix-operation-confirm)
            (y-or-n-p
             (let ((count (length generations)))
               (if (> count 1)
                   (format "Delete %d generations from profile '%s'? "
                           count profile)
                 (format "Delete generation %d from profile '%s'? "
                         (car generations) profile)))))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'delete-generations* profile generations)
     operation-buffer)))

(defun guix-switch-to-generation (profile generation
                                  &optional operation-buffer)
  "Switch PROFILE to GENERATION."
  (when (or (not guix-operation-confirm)
            (y-or-n-p (format "Switch profile '%s' to generation %d? "
                              profile generation)))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'switch-to-generation* profile generation)
     operation-buffer)))

(defun guix-generation-current-package-profile (&optional generation)
  "Return a directory where packages are installed for the
current profile's GENERATION."
  (guix-package-profile (guix-ui-current-profile) generation))


;;; Generation 'info'

(guix-ui-define-interface generation info
  :mode-name "Generation-Info"
  :buffer-name "*Guix Generation Info*"
  :get-entries-function 'guix-generation-info-get-entries
  :format '(guix-generation-info-insert-heading
            nil
            (prev-number format guix-generation-info-insert-previous)
            (current format guix-generation-info-insert-current)
            (number-of-packages format guix-generation-info-insert-packages)
            (file-name simple (indent bui-file))
            (time format (time)))
  :titles '((prev-number . "Prev. generation"))
  :required '(id number))

(defface guix-generation-info-heading
  '((t :inherit bui-info-heading))
  "Face used for generation heading."
  :group 'guix-generation-info-faces)

(defface guix-generation-info-current
  '((t :inherit guix-package-info-installed-outputs))
  "Face used if a generation is the current one."
  :group 'guix-generation-info-faces)

(defface guix-generation-info-not-current
  '((t nil))
  "Face used if a generation is not the current one."
  :group 'guix-generation-info-faces)

(defun guix-generation-info-get-entries (profile search-type
                                                 &rest search-values)
  "Return 'generation' entries for displaying them in 'info' buffer."
  (guix-generation-get-entries
   'generation-sexps
   profile search-type search-values
   (cl-union guix-generation-info-required-params
             (bui-info-displayed-params 'guix-generation))))

(defun guix-generation-info-insert-heading (entry)
  "Insert generation ENTRY heading at point."
  (bui-format-insert
   (concat "Generation "
           (number-to-string (bui-entry-value entry 'number)))
   'guix-generation-info-heading)
  (bui-newline))

(defun guix-generation-info-insert-previous (prev-number entry)
  "Insert PREV-NUMBER and button to compare generations."
  (bui-format-insert prev-number)
  (bui-insert-indent)
  (when (> prev-number 0)
    (let ((number (bui-entry-non-void-value entry 'number)))
      (bui-insert-action-button
       "Compare"
       (lambda (btn)
         (guix-diff
          (guix-profile-generation-packages-buffer
           (button-get btn 'prev-number))
          (guix-profile-generation-packages-buffer
           (button-get btn 'number))))
       (format "Show Diff of packages installed in generations %d and %d"
               prev-number number)
       'prev-number prev-number
       'number number))))

(defun guix-generation-info-insert-packages (number entry)
  "Insert the NUMBER of packages and button to display packages."
  (bui-format-insert number)
  (bui-insert-indent)
  (let ((number (bui-entry-non-void-value entry 'number)))
    (bui-insert-action-button
     "Packages"
     (lambda (btn)
       (guix-package-get-display
        (guix-generation-current-package-profile
         (button-get btn 'number))
        'installed))
     (format "Show packages installed in generation %d" number)
     'number number)))

(defun guix-generation-info-insert-current (val entry)
  "Insert boolean value VAL showing whether this generation is current."
  (if val
      (bui-info-insert-value-format "Yes" 'guix-generation-info-current)
    (bui-info-insert-value-format "No" 'guix-generation-info-not-current)
    (bui-insert-indent)
    (let ((number (bui-entry-non-void-value entry 'number)))
      (bui-insert-action-button
       "Switch"
       (lambda (btn)
         (guix-switch-to-generation (guix-ui-current-profile)
                                    (button-get btn 'number)
                                    (current-buffer)))
       (format "Switch to generation %d (make it the current one)"
               number)
       'number number)
      (bui-insert-indent)
      (bui-insert-action-button
       "Delete"
       (lambda (btn)
         (guix-delete-generations (guix-ui-current-profile)
                                  (list (button-get btn 'number))
                                  (current-buffer)))
       (format "Delete generation %d" number)
       'number number))))


;;; Generation 'list'

(guix-ui-define-interface generation list
  :mode-name "Generation-List"
  :buffer-name "*Guix Generations*"
  :get-entries-function 'guix-generation-list-get-entries
  :describe-function 'guix-ui-list-describe
  :format '((number nil 5 bui-list-sort-numerically-0 :right-align t)
            (current guix-generation-list-get-current 10 t)
            (number-of-packages nil 11 bui-list-sort-numerically-2
                                :right-align t)
            (time bui-list-get-time 20 t)
            (file-name bui-list-get-file-name 30 t))
  :titles '((number . "N.")
            (number-of-packages . "Packages"))
  :hint 'guix-generation-list-hint
  :sort-key '(number . t)
  :marks '((delete . ?D)))

(let ((map guix-generation-list-mode-map))
  (define-key map (kbd "P")   'guix-generation-list-show-packages)
  (define-key map (kbd "+")   'guix-generation-list-show-added-packages)
  (define-key map (kbd "-")   'guix-generation-list-show-removed-packages)
  (define-key map (kbd "=")   'guix-generation-list-diff)
  (define-key map (kbd "e")   'guix-generation-list-ediff)
  (define-key map (kbd "x")   'guix-generation-list-execute)
  (define-key map (kbd "c")   'guix-generation-list-set-current)
  (define-key map (kbd "d")   'guix-generation-list-mark-delete))

(defvar guix-generation-list-default-hint
  '(("\\[guix-generation-list-show-packages]") " show packages;\n"
    ("\\[guix-generation-list-set-current]") " set current generation;\n"
    ("\\[guix-generation-list-diff]") " show Diff of the marked generations;\n"
    ("\\[guix-generation-list-mark-delete]") " mark for deletion; "
    ("\\[guix-generation-list-execute]") " execute operation (deletions);\n"))

(defun guix-generation-list-hint ()
  (bui-format-hints
   guix-generation-list-default-hint
   (bui-default-hint)))

(defun guix-generation-list-get-entries (profile search-type
                                                 &rest search-values)
  "Return 'generation' entries for displaying them in 'list' buffer."
  (guix-generation-get-entries
   'generation-sexps
   profile search-type search-values
   (cl-union guix-generation-list-required-params
             (bui-list-displayed-params 'guix-generation))))

(defun guix-generation-list-get-current (val &optional _)
  "Return string from VAL showing whether this generation is current.
VAL is a boolean value."
  (if val "(current)" ""))

(defun guix-generation-list-set-current ()
  "Switch current profile to the generation at point."
  (interactive)
  (let* ((entry   (bui-list-current-entry))
         (current (bui-entry-non-void-value entry 'current))
         (number  (bui-entry-non-void-value entry 'number)))
    (if current
        (user-error "This generation is already the current one")
      (guix-switch-to-generation (guix-ui-current-profile)
                                 number (current-buffer)))))

(defun guix-generation-list-show-packages ()
  "List installed packages for the generation at point."
  (interactive)
  (guix-package-get-display
   (guix-generation-current-package-profile (bui-list-current-id))
   'installed))

(defun guix-generation-list-generations-to-compare ()
  "Return a sorted list of 2 marked generations for comparing."
  (let ((numbers (bui-list-get-marked-id-list 'general)))
    (if (/= (length numbers) 2)
        (user-error "2 generations should be marked for comparing")
      (sort numbers #'<))))

(defun guix-generation-list-profiles-to-compare ()
  "Return a sorted list of 2 marked generation profiles for comparing."
  (mapcar #'guix-generation-current-package-profile
          (guix-generation-list-generations-to-compare)))

(defun guix-generation-list-show-added-packages ()
  "List package outputs added to the latest marked generation.
If 2 generations are marked with \\[guix-list-mark], display
outputs installed in the latest marked generation that were not
installed in the other one."
  (interactive)
  (bui-get-display-entries
   'guix-output 'list
   (cl-list* (guix-ui-current-profile)
             'profile-diff
             (reverse (guix-generation-list-profiles-to-compare)))
   'add))

(defun guix-generation-list-show-removed-packages ()
  "List package outputs removed from the latest marked generation.
If 2 generations are marked with \\[guix-list-mark], display
outputs not installed in the latest marked generation that were
installed in the other one."
  (interactive)
  (bui-get-display-entries
   'guix-output 'list
   (cl-list* (guix-ui-current-profile)
             'profile-diff
             (guix-generation-list-profiles-to-compare))
   'add))

(defun guix-generation-list-compare (diff-fun gen-fun)
  "Run GEN-FUN on the 2 marked generations and run DIFF-FUN on the results."
  (cl-multiple-value-bind (gen1 gen2)
      (guix-generation-list-generations-to-compare)
    (funcall diff-fun
             (funcall gen-fun gen1)
             (funcall gen-fun gen2))))

(defun guix-generation-list-ediff-manifests ()
  "Run Ediff on manifests of the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'ediff-files
   #'guix-profile-generation-manifest-file))

(defun guix-generation-list-diff-manifests ()
  "Run Diff on manifests of the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'guix-diff
   #'guix-profile-generation-manifest-file))

(defun guix-generation-list-ediff-packages ()
  "Run Ediff on package outputs installed in the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'ediff-buffers
   #'guix-profile-generation-packages-buffer))

(defun guix-generation-list-diff-packages ()
  "Run Diff on package outputs installed in the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'guix-diff
   #'guix-profile-generation-packages-buffer))

(defun guix-generation-list-ediff (arg)
  "Run Ediff on package outputs installed in the 2 marked generations.
With ARG, run Ediff on manifests of the marked generations."
  (interactive "P")
  (if arg
      (guix-generation-list-ediff-manifests)
    (guix-generation-list-ediff-packages)))

(defun guix-generation-list-diff (arg)
  "Run Diff on package outputs installed in the 2 marked generations.
With ARG, run Diff on manifests of the marked generations."
  (interactive "P")
  (if arg
      (guix-generation-list-diff-manifests)
    (guix-generation-list-diff-packages)))

(defun guix-generation-list-mark-delete (&optional arg)
  "Mark the current generation for deletion and move to the next line.
With ARG, mark all generations for deletion."
  (interactive "P")
  (if arg
      (bui-list-mark-all 'delete)
    (bui-list--mark 'delete t)))

(defun guix-generation-list-execute ()
  "Delete marked generations."
  (interactive)
  (let ((marked (bui-list-get-marked-id-list 'delete)))
    (or marked
        (user-error "No generations marked for deletion"))
    (guix-delete-generations (guix-ui-current-profile)
                             marked (current-buffer))))


;;; Inserting packages to compare generations

(defcustom guix-generation-packages-buffer-name-function
  #'guix-generation-packages-buffer-name-default
  "Function used to define name of a buffer with generation packages.
This function is called with 2 arguments: PROFILE (string) and
GENERATION (number)."
  :type '(choice (function-item guix-generation-packages-buffer-name-default)
                 (function-item guix-generation-packages-buffer-name-long)
                 (function :tag "Other function"))
  :group 'guix-generation)

(defcustom guix-generation-packages-update-buffer t
  "If non-nil, always update list of packages during comparing generations.
If nil, generation packages are received only once.  So when you
compare generation 1 and generation 2, the packages for both
generations will be received.  Then if you compare generation 1
and generation 3, only the packages for generation 3 will be
received.  Thus if you use comparing of different generations a
lot, you may set this variable to nil to improve the
performance."
  :type 'boolean
  :group 'guix-generation)

(defvar guix-generation-output-name-width 30
  "Width of an output name \"column\".
This variable is used in auxiliary buffers for comparing generations.")

(defun guix-generation-packages (profile)
  "Return a list of sorted packages installed in PROFILE.
Each element of the list is a list of the package specification
and its store file name."
  (sort (guix-eval-read
         (guix-make-guile-expression
          'profile->specifications+file-names profile))
        (lambda (a b)
          (string< (car a) (car b)))))

(defun guix-generation-packages-buffer-name-default (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs.
Use base name of PROFILE file name."
  (let ((profile-name (file-name-base (directory-file-name profile))))
    (format "*Guix %s: generation %s*"
            profile-name generation)))

(defun guix-generation-packages-buffer-name-long (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs.
Use the full PROFILE file name."
  (format "*Guix generation %s (%s)*"
          generation profile))

(defun guix-generation-packages-buffer-name (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs."
  (funcall guix-generation-packages-buffer-name-function
           profile generation))

(defun guix-generation-insert-package (name file-name)
  "Insert package output NAME and store FILE-NAME at point."
  (insert name)
  (indent-to guix-generation-output-name-width 2)
  (insert file-name "\n"))

(defun guix-generation-insert-packages (buffer profile)
  "Insert package outputs installed in PROFILE in BUFFER."
  (with-current-buffer buffer
    (setq buffer-read-only nil
          indent-tabs-mode nil)
    (erase-buffer)
    (mapc (-lambda ((name file-name))
            (guix-generation-insert-package name file-name))
          (guix-generation-packages profile))))

(defun guix-generation-packages-buffer (profile generation)
  "Return buffer with package outputs installed in PROFILE's GENERATION.
Create the buffer if needed."
  (let ((buf-name (guix-generation-packages-buffer-name
                   profile generation)))
    (or (and (null guix-generation-packages-update-buffer)
             (get-buffer buf-name))
        (let ((buf (get-buffer-create buf-name)))
          (guix-generation-insert-packages
           buf
           (guix-package-profile profile generation))
          buf))))

(defun guix-profile-generation-manifest-file (generation)
  "Return the file name of a GENERATION's manifest.
GENERATION is a generation number of the current profile."
  (guix-manifest-file (guix-ui-current-profile) generation))

(defun guix-profile-generation-packages-buffer (generation)
  "Insert GENERATION's package outputs in a buffer and return it.
GENERATION is a generation number of the current profile."
  (guix-generation-packages-buffer (guix-ui-current-profile)
                                   generation))


;;; Interactive commands

;;;###autoload
(defun guix-generations (&optional profile)
  "Display information about all generations.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-generation-profile)))
  (guix-generation-get-display profile 'all))

;;;###autoload
(defun guix-last-generations (number &optional profile)
  "Display information about last NUMBER generations.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-number "The number of last generations: ")
         (guix-ui-read-generation-profile)))
  (guix-generation-get-display profile 'last number))

;;;###autoload
(defun guix-generations-by-time (from to &optional profile)
  "Display information about generations created between FROM and TO.
FROM and TO should be time values.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-date "Find generations (from): ")
         (guix-read-date "Find generations (to): ")
         (guix-ui-read-generation-profile)))
  (guix-generation-get-display profile 'time
                               (float-time from)
                               (float-time to)))

(provide 'guix-ui-generation)

;;; guix-ui-generation.el ends here
