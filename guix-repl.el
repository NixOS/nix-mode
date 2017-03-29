;;; guix-repl.el --- Making and using Guix REPL

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

;; This file provides the code for interacting with Guile using Guix REPL
;; (Geiser REPL with some guix-specific additions).

;; By default (if `guix-repl-use-server' is non-nil) 2 Guix REPLs are
;; started.  The main one (with "guile --listen" process) is used for
;; "interacting" with a user - for showing a progress of
;; installing/deleting Guix packages.  The second (internal) REPL is
;; used for synchronous evaluating, e.g. when information about
;; packages/generations should be received for a list/info buffer.
;;
;; This "2 REPLs concept" makes it possible to have a running process of
;; installing/deleting packages and to continue to search/list/get info
;; about other packages at the same time.  If you prefer to use a single
;; Guix REPL, do not try to receive any information while there is a
;; running code in the REPL (see
;; <https://github.com/jaor/geiser/issues/28>).
;;
;; Guix REPLs (unlike the usual Geiser REPLs) are not added to
;; `geiser-repl--repls' variable, and thus cannot be used for evaluating
;; while editing .scm files.  The only purpose of Guix REPLs is to be an
;; intermediate between "Guix/Guile level" and "Emacs interface level".
;; That being said, you can still wish to use a Guix REPL while hacking
;; Scheme files of Emacs-Guix.  It is possible: you can just use
;; `geiser-connect-local' command with `guix-repl-current-socket' to
;; have a usual Geiser REPL with all stuff defined by Emacs-Guix
;; package (it is placed in (emacs-guix) module).

;;; Code:

(require 'dash)
(require 'geiser-mode)
(require 'geiser-guile)
(require 'guix nil t)
(require 'guix-geiser)
(require 'guix-config)
(require 'guix-external)
(require 'guix-profiles)
(require 'guix-utils)

(defvar guix-load-path nil
  "List of directories prepended to Guile's `%load-path' when
Guix REPL is started.

Most likely you don't need to set this variable, but if you
really do, note that these directories take precedence over any
other added directory (including Guile modules of Emacs-Guix and
Guix itself).

Directories are used as is, without expanding, so make sure they
do not contain things like \"~\" or \"..\" (use
`expand-file-name').

These directories are also prepended to `%load-compiled-path'
unless `guix-load-compiled-path' is specified.")

(defvar guix-load-compiled-path nil
  "List of directories prepended to Guile's `%load-compiled-path'
when Guix REPL is started.

See `guix-load-path' for details.")


;;; REPL

(defgroup guix-repl nil
  "Settings for Guix REPLs."
  :prefix "guix-repl-"
  :group 'guix)

(defcustom guix-repl-startup-time geiser-repl-startup-time
  "Time, in milliseconds, to wait for Guix REPL to startup.
Same as `geiser-repl-startup-time' but is used for Guix REPL.
If you have a slow system, try to increase this time."
  :type 'integer
  :group 'guix-repl)

(defcustom guix-repl-buffer-name "*Guix REPL*"
  "Default name of a Geiser REPL buffer used for Guix."
  :type 'string
  :group 'guix-repl)

(defcustom guix-repl-after-start-hook nil
  "Hook called after Guix REPL is started."
  :type 'hook
  :group 'guix-repl)

(define-obsolete-variable-alias 'guix-use-guile-server
  'guix-repl-use-server "0.2")

(defcustom guix-repl-use-server t
  "If non-nil, start guile with '--listen' argument.
This allows to receive information about packages using an
additional (so called 'internal') REPL while some packages are
being installed/removed in the main Guix REPL."
  :type 'boolean
  :group 'guix-repl)

(defcustom guix-repl-use-latest t
  "If non-nil, use \"~/.config/guix/latest\" directory.
It contains the latest Guix code populated after running \"guix pull\"."
  :type 'boolean
  :group 'guix-repl)

(defcustom guix-repl-socket-file-name-function
  #'guix-repl-socket-file-name
  "Function used to define a socket file name used by Guix REPL.
The function is called without arguments."
  :type '(choice (function-item guix-repl-socket-file-name)
                 (function :tag "Other function"))
  :group 'guix-repl)

(defcustom guix-emacs-activate-after-operation t
  "Activate Emacs packages after installing.
If nil, do not load autoloads of the Emacs packages after
they are successfully installed."
  :type 'boolean
  :group 'guix-repl)

(defvar guix-repl-default-directory
  ;; If the current buffer is "tramp"-ed, the REPL may be started
  ;; with the root rights!  So make sure `default-directory' is
  ;; "safe".  See <https://notabug.org/alezost/emacs-guix/issues/1>
  ;; for details.
  (or (getenv "HOME")
      user-emacs-directory)
  "Guix REPL is started with this directory as `default-directory'.")

(defvar guix-repl-current-socket nil
  "Name of a socket file used by the current Guix REPL.")

(defvar guix-repl-buffer nil
  "Main Geiser REPL buffer used for communicating with Guix.
This REPL is used for processing package actions and for
receiving information if `guix-repl-use-server' is nil.")

(defvar guix-internal-repl-buffer nil
  "Additional Geiser REPL buffer used for communicating with Guix.
This REPL is used for receiving information only if
`guix-repl-use-server' is non-nil.")

(defvar guix-internal-repl-buffer-name "*Guix Internal REPL*"
  "Default name of an internal Guix REPL buffer.")

(defvar guix-repl-before-operation-hook nil
  "Hook run before executing an operation in Guix REPL.")

(defvar guix-repl-after-operation-hook
  '(guix-update-buffers-after-operation
    guix-repl-autoload-emacs-packages-maybe
    guix-repl-operation-success-message)
  "Hook run after executing successful operation in Guix REPL.")

(defvar guix-repl-operation-p nil
  "Non-nil, if current operation is performed by `guix-eval-in-repl'.
This internal variable is used to distinguish Guix operations
from operations performed in Guix REPL by a user.")

(defvar guix-repl-operation-type nil
  "Type of the current operation performed by `guix-eval-in-repl'.
This internal variable is used to define what actions should be
executed after the current operation succeeds.
See `guix-eval-in-repl' for details.")

(declare-function guix-emacs-autoload-packages "guix-emacs" t)

(defun guix-repl-autoload-emacs-packages-maybe ()
  "Load autoloads for Emacs packages if needed.
See `guix-emacs-activate-after-operation' for details."
  (and guix-emacs-activate-after-operation
       (require 'guix-emacs nil t)
       ;; FIXME Since a user can work with a non-current profile (using
       ;; C-u before `guix-search-by-name' and other commands), emacs
       ;; packages can be installed to another profile, and the
       ;; following code will not work (i.e., the autoloads for this
       ;; profile will not be loaded).
       (guix-emacs-autoload-packages guix-current-profile)))

(defun guix-repl-operation-success-message ()
  "Message telling about successful Guix operation."
  (message "Guix operation has been performed."))

(defun guix-repl-guile-args ()
  "Return a list of Guile's arguments to start Guix REPL."
  `(,@(and guix-load-path
           (let* ((lp  (guix-list-maybe guix-load-path))
                  (lcp (if guix-load-compiled-path
                           (guix-list-maybe guix-load-compiled-path)
                         lp)))
             (append (--mapcat (list "-L" it) lp)
                     (--mapcat (list "-C" it) lcp))))
    "-L" ,guix-scheme-directory
    ,@(and guix-config-scheme-compiled-directory
           (list "-C" guix-config-scheme-compiled-directory))
    ,@(and guix-repl-use-latest
           (let ((latest-dir (guix-latest-directory)))
             (list "-L" latest-dir
                   "-C" latest-dir)))
    ,@(and guix-config-guix-scheme-directory
           (list "-L" guix-config-guix-scheme-directory
                 "-C" (or guix-config-guix-scheme-compiled-directory
                          guix-config-guix-scheme-directory)))
    ,@(and guix-repl-use-server
           (list (concat "--listen=" guix-repl-current-socket)))))

(defun guix-repl-guile-program (&optional internal)
  "Return a value suitable for `geiser-guile-binary' to start Guix REPL.
If INTERNAL is non-nil, return the value for the internal Guix REPL."
  (if internal
      guix-guile-program
    (append (guix-list-maybe guix-guile-program)
            (guix-repl-guile-args))))

(defun guix-repl-socket-file-name ()
  "Return a fresh name of a socket file used by Guix REPL."
  (guix-temporary-file-name "repl-socket"))

(defun guix-repl-delete-socket-maybe ()
  "Delete `guix-repl-current-socket' file if it exists."
  (and guix-repl-current-socket
       (file-exists-p guix-repl-current-socket)
       (delete-file guix-repl-current-socket)))

(defun guix-start-process-maybe (&optional start-msg end-msg)
  "Start Geiser REPL configured for Guix if needed.
START-MSG and END-MSG are strings displayed in the minibuffer in
the beginning and in the end of the starting process.  If nil,
display default messages."
  (guix-start-repl-maybe nil
                         (or start-msg "Starting Guix REPL ...")
                         end-msg)
  (if guix-repl-use-server
      (guix-start-repl-maybe 'internal)
    (setq guix-internal-repl-buffer guix-repl-buffer)))

(defun guix-start-repl-maybe (&optional internal start-msg end-msg)
  "Start Guix REPL if needed.
If INTERNAL is non-nil, start an internal REPL.

START-MSG and END-MSG are strings displayed in the minibuffer in
the beginning and in the end of the process.  If nil, do not
display messages."
  (let* ((repl-var (guix-get-repl-buffer-variable internal))
         (repl (symbol-value repl-var))
         (default-directory guix-repl-default-directory))
    (unless (and (buffer-live-p repl)
                 (get-buffer-process repl))
      (and start-msg (message start-msg))
      (setq guix-repl-operation-p nil)
      (unless internal
        ;; Guile leaves socket file after exit, so remove it if it
        ;; exists (after the REPL restart).
        (guix-repl-delete-socket-maybe)
        (setq guix-repl-current-socket
              (and guix-repl-use-server
                   (or guix-repl-current-socket
                       (funcall guix-repl-socket-file-name-function)))))
      (let ((geiser-guile-binary (guix-repl-guile-program internal))
            (repl (get-buffer-create
                   (guix-get-repl-buffer-name internal))))
        (guix-start-repl repl (and internal guix-repl-current-socket))
        (set repl-var repl)
        ;; Wait until switching to (emacs-guix) module finishes.
        (guix-geiser-eval-in-repl-synchronously
         ",m (emacs-guix)" repl t t)
        (and end-msg (message end-msg))
        (unless internal
          (run-hooks 'guix-repl-after-start-hook))))))

(defun guix-start-repl (buffer &optional address)
  "Start Guix REPL in BUFFER.
If ADDRESS is non-nil, connect to a remote guile process using
this address (it should be defined by
`geiser-repl--read-address')."
  ;; A mix of the code from `geiser-repl--start-repl' and
  ;; `geiser-repl--to-repl-buffer'.
  (let ((impl 'guile)
        (geiser-repl-startup-time guix-repl-startup-time))
    (with-current-buffer buffer
      (geiser-repl-mode)
      (geiser-impl--set-buffer-implementation impl)
      (geiser-repl--autodoc-mode -1)
      (goto-char (point-max))
      (let ((prompt (geiser-con--combined-prompt
                     geiser-guile--prompt-regexp
                     geiser-guile--debugger-prompt-regexp)))
        (geiser-repl--save-remote-data address)
        (geiser-repl--start-scheme impl address prompt)
        (geiser-repl--quit-setup)
        (geiser-repl--history-setup)
        (setq-local geiser-repl--repls (list buffer))
        (geiser-repl--set-this-buffer-repl buffer)
        (setq geiser-repl--connection
              (geiser-con--make-connection
               (get-buffer-process (current-buffer))
               geiser-guile--prompt-regexp
               geiser-guile--debugger-prompt-regexp))
        (geiser-repl--startup impl address)
        (geiser-repl--autodoc-mode 1)
        (geiser-company--setup geiser-repl-company-p)
        (add-hook 'comint-output-filter-functions
                  'guix-repl-output-filter
                  nil t)
        (set-process-query-on-exit-flag
         (get-buffer-process (current-buffer))
         geiser-repl-query-on-kill-p)))))

(defun guix-repl-output-filter (str)
  "Filter function suitable for `comint-output-filter-functions'.
This is a replacement for `geiser-repl--output-filter'."
  (cond
   ((string-match-p geiser-guile--prompt-regexp str)
    (geiser-autodoc--disinhibit-autodoc)
    (when guix-repl-operation-p
      (setq guix-repl-operation-p nil)
      (run-hooks 'guix-repl-after-operation-hook)
      ;; Run hooks specific to the current operation type.
      (when guix-repl-operation-type
        (let ((type-hook (intern
                          (concat "guix-after-"
                                  (symbol-name guix-repl-operation-type)
                                  "-hook"))))
          (setq guix-repl-operation-type nil)
          (and (boundp type-hook)
               (run-hooks type-hook))))))
   ((string-match geiser-guile--debugger-prompt-regexp str)
    (setq guix-repl-operation-p nil)
    (geiser-con--connection-set-debugging geiser-repl--connection
                                          (match-beginning 0))
    (geiser-autodoc--disinhibit-autodoc))))

(defun guix-repl-exit (&optional internal no-wait)
  "Exit the current Guix REPL.
If INTERNAL is non-nil, exit the internal REPL.
If NO-WAIT is non-nil, do not wait for the REPL process to exit:
send a kill signal to it and return immediately."
  (let ((repl (symbol-value (guix-get-repl-buffer-variable internal))))
    (when (get-buffer-process repl)
      (with-current-buffer repl
        (geiser-con--connection-deactivate geiser-repl--connection t)
        (comint-kill-subjob)
        (unless no-wait
          (while (get-buffer-process repl)
            (sleep-for 0.1)))))))

(defun guix-get-repl-buffer (&optional internal)
  "Return Guix REPL buffer; start REPL if needed.
If INTERNAL is non-nil, return an additional internal REPL."
  (guix-start-process-maybe)
  (let ((repl (symbol-value (guix-get-repl-buffer-variable internal))))
    ;; If a new Geiser REPL is started, `geiser-repl--repl' variable may
    ;; be set to the new value in a Guix REPL, so set it back to a
    ;; proper value here.
    (with-current-buffer repl
      (geiser-repl--set-this-buffer-repl repl))
    repl))

(defun guix-get-repl-buffer-variable (&optional internal)
  "Return the name of a variable with a REPL buffer."
  (if internal
      'guix-internal-repl-buffer
    'guix-repl-buffer))

(defun guix-get-repl-buffer-name (&optional internal)
  "Return the name of a REPL buffer."
  (if internal
      guix-internal-repl-buffer-name
    guix-repl-buffer-name))

(defun guix-switch-to-repl (&optional internal)
  "Switch to Guix REPL.
If INTERNAL is non-nil (interactively with prefix), switch to the
additional internal REPL if it exists."
  (interactive "P")
  (geiser-repl--switch-to-buffer (guix-get-repl-buffer internal)))


;;; Guix directory

(defvar guix-directory nil
  "Default directory with Guix source.
If it is not set by a user, it is set after starting Guix REPL.
This directory is used to find packages and licenses by such
commands as `guix-edit' or `guix-find-license-definition'.")

(defun guix-read-directory ()
  "Return `guix-directory' or prompt for it.
This function is intended for using in `interactive' forms."
  (if current-prefix-arg
      (read-directory-name "Directory with Guix modules: "
                           guix-directory)
    guix-directory))

(defun guix-latest-directory ()
  "Return 'guix pull'-ed directory."
  (let* ((config-dir (or (getenv "XDG_CONFIG_HOME")
                         (expand-file-name "~/.config")))
         (latest-dir (expand-file-name "guix/latest" config-dir)))
    (if (file-exists-p latest-dir)
        (or guix-directory
            (setq guix-directory latest-dir))
      (message "Directory '%s' does not exist.
Consider running \"guix pull\"." latest-dir))
    latest-dir))


;;; Operation buffers

(define-obsolete-variable-alias 'guix-ui-update-after-operation
  'guix-update-buffers-after-operation "0.2")

(defcustom guix-update-buffers-after-operation 'current
  "Define what kind of data to update after executing an operation.

After successful executing of some operation in the Guix
REPL (for example after installing a package), the data in Guix
buffers will or will not be automatically updated depending on a
value of this variable.

If nil, update nothing (do not revert any buffer).
If `current', update the buffer from which an operation was performed.
If `all', update all Guix buffers (not recommended)."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Update operation buffer" current)
                 (const :tag "Update all Guix buffers" all))
  :group 'guix-repl)

(defvar guix-operation-buffer nil
  "Buffer from which the latest Guix operation was performed.")

(defun guix-operation-buffer? (&optional buffer modes)
  "Return non-nil if BUFFER mode is derived from any of the MODES.
If BUFFER is nil, check current buffer.
If MODES is nil, use modes for Guix package management."
  (with-current-buffer (or buffer (current-buffer))
    (apply #'derived-mode-p
           (or modes '(guix-package-list-mode
                       guix-package-info-mode
                       guix-output-list-mode
                       guix-profile-list-mode
                       guix-generation-list-mode
                       guix-generation-info-mode)))))

(defun guix-operation-buffers (&optional modes)
  "Return a list of all buffers with major modes derived from MODES.
If MODES is nil, return list of all Guix 'list' and 'info' buffers."
  (--filter (guix-operation-buffer? it modes)
            (buffer-list)))

(defun guix-update-buffers-after-operation ()
  "Update buffers after Guix operation if needed.
See `guix-update-after-operation' for details."
  (let ((to-update
         (and guix-operation-buffer
              (cl-case guix-update-buffers-after-operation
                (current (and (buffer-live-p guix-operation-buffer)
                              (guix-operation-buffer?
                               guix-operation-buffer)
                              (list guix-operation-buffer)))
                (all     (guix-operation-buffers))))))
    (setq guix-operation-buffer nil)
    (dolist (buffer to-update)
      (with-current-buffer buffer
        (revert-buffer nil t)))))


;;; Evaluating expressions

(defun guix-eval (str)
  "Evaluate STR with guile expression using Guix REPL.
See `guix-geiser-eval' for details."
  (guix-geiser-eval str (guix-get-repl-buffer 'internal)))

(defun guix-eval-read (str)
  "Evaluate STR with guile expression using Guix REPL.
See `guix-geiser-eval-read' for details."
  (guix-geiser-eval-read str (guix-get-repl-buffer 'internal)))

(defun guix-eval-in-repl (str &optional operation-buffer operation-type)
  "Switch to Guix REPL and evaluate STR with guile expression there.
If OPERATION-BUFFER is non-nil, it should be a buffer from which
the current operation was performed.

If OPERATION-TYPE is non-nil, it should be a symbol.  After
successful executing of the current operation,
`guix-after-OPERATION-TYPE-hook' is called."
  (run-hooks 'guix-repl-before-operation-hook)
  (setq guix-repl-operation-p t
        guix-repl-operation-type operation-type
        guix-operation-buffer operation-buffer)
  (guix-geiser-eval-in-repl str (guix-get-repl-buffer)))

(provide 'guix-repl)

;;; guix-repl.el ends here
