;;; guix-ui.el --- Common code for Guix package management interface  -*- lexical-binding: t -*-

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

;; This file provides some general code for 'list'/'info' interfaces for
;; packages and generations.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'guix nil t)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-utils)
(require 'guix-ui-messages)
(require 'guix-profiles)

(defvar guix-ui-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M") 'guix-apply-manifest)
    (define-key map (kbd "C-c C-z") 'guix-switch-to-repl)
    map)
  "Parent keymap for Guix package/generation buffers.")

(bui-define-current-args-accessors
 guix-ui-current profile search-type search-values)

(defun guix-ui-read-package-profile ()
  "Return `guix-current-profile' or prompt for it.
This function is intended for using in `interactive' forms."
  (if current-prefix-arg
      (guix-read-package-profile)
    guix-current-profile))

(defun guix-ui-read-generation-profile ()
  "Return `guix-current-profile' or prompt for it.
This function is intended for using in `interactive' forms."
  (if current-prefix-arg
      (guix-read-generation-profile)
    guix-current-profile))

(defun guix-ui-get-entries (profile entry-type search-type search-values
                                    &optional params)
  "Receive ENTRY-TYPE entries for PROFILE.
Call an appropriate scheme procedure and return a list of entries.

ENTRY-TYPE should be one of the following symbols: `package' or
`output'.

SEARCH-TYPE may be one of the following symbols: `id', `name',
`regexp', `all-available', `newest-available', `installed',
`obsolete', `license', `location', `from-file', `from-os-file'.

PARAMS is a list of parameters for receiving.  If nil, get data
with all available parameters."
  (guix-eval-read
   (guix-make-guile-expression
    'package/output-sexps
    profile entry-type search-type search-values params)))

(defun guix-ui-list-describe (&rest ids)
  "Describe 'ui' entries with IDS (list of identifiers)."
  (bui-get-display-entries
   (bui-current-entry-type) 'info
   (cl-list* (guix-ui-current-profile) 'id ids)
   'add))


;;; Buffers

(defcustom guix-ui-buffer-name-function
  #'guix-ui-buffer-name-full
  "Function used to define a name of a Guix buffer.
The function is called with 2 arguments: BASE-NAME and PROFILE."
  :type '(choice (function-item guix-ui-buffer-name-full)
                 (function-item guix-ui-buffer-name-short)
                 (function-item guix-ui-buffer-name-simple)
                 (function :tag "Other function"))
  :group 'guix)

(defun guix-ui-buffer-name-simple (base-name &rest _)
  "Return BASE-NAME."
  base-name)

(defun guix-ui-buffer-name-short (base-name profile)
  "Return buffer name by appending BASE-NAME and PROFILE's base file name."
  (guix-compose-buffer-name base-name
                            (file-name-base (directory-file-name profile))))

(defun guix-ui-buffer-name-full (base-name profile)
  "Return buffer name by appending BASE-NAME and PROFILE's full name."
  (guix-compose-buffer-name base-name profile))

(defun guix-ui-buffer-name (base-name profile)
  "Return Guix buffer name based on BASE-NAME and profile.
See `guix-ui-buffer-name-function' for details."
  (funcall guix-ui-buffer-name-function
           base-name profile))


;;; Interface definers

(defmacro guix-ui-define-entry-type (entry-type &rest args)
  "Define general code for ENTRY-TYPE.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
They are passed to `bui-define-entry-type' macro.

This macro also defines:

  - `guix-TYPE-message' - a wrapper around `guix-result-message'."
  (declare (indent 1))
  (let* ((entry-type-str  (symbol-name entry-type))
         (full-entry-type (guix-make-symbol entry-type))
         (prefix          (concat "guix-" entry-type-str))
         (message-fun     (intern (concat prefix "-message"))))
    `(progn
       (defun ,message-fun (entries profile search-type
                                    &rest search-values)
         ,(format "\
Display a message after showing '%s' entries.
This is a wrapper for `guix-result-message'."
                  entry-type-str)
         (guix-result-message profile entries ',entry-type
                              search-type search-values))

       (guix-define-groups ,entry-type)

       (bui-define-entry-type ,full-entry-type
         :message-function ',message-fun
         ,@args))))

(defmacro guix-ui-define-interface (entry-type buffer-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
In the following description TYPE means ENTRY-TYPE-BUFFER-TYPE.

Required keywords:

  - `:buffer-name' - base part of a buffer name.  It is used in a
    generated `guix-TYPE-buffer-name' function; see
    `guix-ui-buffer-name' for details.

Optional keywords:

  - `:required' - default value of the generated
    `guix-TYPE-required-params' variable.

The rest keyword arguments are passed to `bui-define-interface'
macro.

Along with the mentioned definitions, this macro also defines:

  - `guix-TYPE-mode-map' - keymap based on `guix-ui-map' and
    `bui-BUFFER-TYPE-mode-map'."
  (declare (indent 2))
  (let* ((entry-type-str  (symbol-name entry-type))
         (buffer-type-str (symbol-name buffer-type))
         (prefix          (concat "guix-" entry-type-str "-"
                                  buffer-type-str))
         (mode-str        (concat prefix "-mode"))
         (mode-map        (intern (concat mode-str "-map")))
         (parent-map      (intern (format "bui-%s-mode-map"
                                          buffer-type-str)))
         (required-var    (intern (concat prefix "-required-params")))
         (buffer-name-fun (intern (concat prefix "-buffer-name"))))
    (bui-plist-let args
        ((buffer-name-val :buffer-name)
         (required-val    :required ''(id)))
      `(progn
         (defvar ,mode-map
           (let ((map (make-sparse-keymap)))
             (set-keymap-parent
              map (make-composed-keymap ,parent-map guix-ui-map))
             map)
           ,(format "Keymap for `%s' buffers." mode-str))

         (defvar ,required-var ,required-val
           ,(format "\
List of the required '%s' parameters.
These parameters are received from the Scheme side
along with the displayed parameters.

Do not remove `id' from this list as it is required for
identifying an entry."
                    entry-type-str))

         (defun ,buffer-name-fun (profile &rest _)
           ,(format "\
Return a name of '%s' buffer for displaying '%s' entries.
See `guix-ui-buffer-name' for details."
                    buffer-type-str entry-type-str)
           (guix-ui-buffer-name ,buffer-name-val profile))

         (bui-define-interface ,(guix-make-symbol entry-type) ,buffer-type
           :buffer-name ',buffer-name-fun
           ,@%foreign-args)))))


(defvar guix-ui-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "guix-ui-define-entry-type"
                           "guix-ui-define-interface"))
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode guix-ui-font-lock-keywords)

(provide 'guix-ui)

;;; guix-ui.el ends here
