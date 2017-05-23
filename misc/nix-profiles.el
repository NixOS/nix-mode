;;; nix-profiles.el --- Guix profiles

;; Copyright © 2014–2017 Alex Kost <alezost@gmail.com>
;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>

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

;; This file provides a general code related to location and contents of
;; Guix profiles.

;;; Code:

(require 'nix-utils)

(defvar nix-state-directory
  ("/nix/var/nix"))

(defvar nix-user-profile
  (expand-file-name "~/.nix-profile")
  "User profile.")

(defvar nix-system-profile
  (concat nix-state-directory "/profiles/system")
  "System profile.")

(defvar nix-default-profile
  (concat nix-state-directory
          "/profiles/per-user/"
          (getenv "USER")
          "/profile")
  "Default Nix profile.")

(defvar nix-current-profile nix-default-profile
  "Current Nix profile.
It is used by various commands as the default working profile.")

(defvar nix-system-profile-regexp
  (rx-to-string `(and string-start
                      (or ,nix-system-profile
                          "/run/booted-system"
                          "/run/current-system"))
                t)
  "Regexp matching system profiles.")

(defun nix-current-profile? (profile)
  "Return non-nil, if package PROFILE is `nix-current-profile'."
  (string= (nix-package-profile profile)
           nix-current-profile))

(defun nix-system-profile? (profile)
  "Return non-nil, if PROFILE is a system one."
  (string-match-p nix-system-profile-regexp profile))

(defun nix-assert-non-system-profile (profile)
  "Raise an error when PROFILE is a system one."
  (when (nix-system-profile? profile)
    (user-error "\
Packages cannot be installed or removed to/from profile '%s'.
Use 'guix system reconfigure' shell command to modify a system profile."
                profile)))

(defun nix-generation-file (profile generation)
  "Return the file name of a PROFILE's GENERATION."
  (format "%s-%s-link" profile generation))

(defun nix-profile (profile)
  "Return normalized file name of PROFILE.
\"Normalized\" means the returned file name is expanded, does not
have a trailing slash and it is `nix-default-profile' if PROFILE
is `nix-user-profile'.  `nix-user-profile' is special because
it is actually a symlink to a real user profile, and the HOME
directory does not contain profile generations."
  (let ((profile (directory-file-name (expand-file-name profile))))
    (if (string= profile nix-user-profile)
        nix-default-profile
      profile)))

(defun nix-generation-profile (profile &optional generation)
  "Return file name of PROFILE or its GENERATION.
The returned file name is the one that have generations in the
same parent directory.

If PROFILE matches `nix-system-profile-regexp', then it is
considered to be a system profile.  Unlike usual profiles, for a
system profile, packages are placed in 'profile' sub-directory,
so the returned file name does not contain this potential
trailing '/profile'."
  (let* ((profile (nix-profile profile))
         (profile (if (and (nix-system-profile? profile)
                           (string-match (rx (group (* any))
                                             "/profile" string-end)
                                         profile))
                      (match-string 1 profile)
                    profile)))
    (if generation
        (nix-generation-file profile generation)
      profile)))

(defun nix-package-profile (profile &optional generation)
  "Return file name of PROFILE or its GENERATION.
The returned file name is the one where packages are installed.

If PROFILE is a system one (see `nix-generation-profile'), then
the returned file name ends with '/profile'."
  (let* ((profile (nix-generation-profile profile))
         (profile (if generation
                      (nix-generation-file profile generation)
                    profile)))
    (if (nix-system-profile? profile)
        (expand-file-name "profile" profile)
      profile)))

(defun nix-manifest-file (profile &optional generation)
  "Return manifest file name of PROFILE or its GENERATION."
  (expand-file-name "manifest"
                    (nix-package-profile profile generation)))

(defun nix-profile-number-of-packages (profile &optional generation)
  "Return the number of packages installed in PROFILE or its GENERATION."
  (let ((manifest (nix-manifest-file profile generation)))
    ;; Just count a number of sexps inside (packages ...) of manifest
    ;; file.  It should be much faster than running the REPL and
    ;; calculating manifest entries on the Scheme side.
    (when (file-exists-p manifest)
      (with-temp-buffer
        (insert-file-contents-literally manifest)
        (goto-char (point-min))
        (re-search-forward "(packages" nil t)
        (down-list)
        (let ((num 0)
              (pos (point)))
          (while (setq pos (condition-case nil
                               (scan-sexps pos 1)
                             (error nil)))
            (setq num (1+ num)))
          num)))))

(defun nix-profile-number-of-generations (profile)
  "Return the number of generations of PROFILE."
  (let* ((profile   (nix-generation-profile profile))
         (dir-name  (file-name-directory profile))
         (base-name (file-name-nondirectory profile))
         (regexp    (concat (regexp-quote base-name)
                            "-[[:digit:]]+-link")))
    (when (file-exists-p profile)
      (length (directory-files dir-name nil regexp 'no-sort)))))


;;; Minibuffer readers

(defun nix-read-profile (&optional default)
  "Prompt for profile and return it.
Use DEFAULT as a start directory.  If it is nil, use
`nix-current-profile'."
  (nix-read-file-name "Profile: "
                      (file-name-directory
                       (or default nix-current-profile))))

(defun nix-read-package-profile (&optional default)
  "Prompt for a package profile and return it.
See `nix-read-profile' for the meaning of DEFAULT, and
`nix-package-profile' for the meaning of package profile."
  (nix-package-profile (nix-read-profile default)))

(defun nix-read-generation-profile (&optional default)
  "Prompt for a generation profile and return it.
See `nix-read-profile' for the meaning of DEFAULT, and
`nix-generation-profile' for the meaning of generation profile."
  (nix-generation-profile (nix-read-profile default)))


;;;###autoload
(defun nix-set-current-profile (file-name)
  "Set `nix-current-profile' to FILE-NAME.
Interactively, prompt for FILE-NAME.  With prefix, use
`nix-default-profile'."
  (interactive
   (list (if current-prefix-arg
             nix-default-profile
           (nix-read-package-profile))))
  (setq nix-current-profile file-name)
  (message "Current profile has been set to '%s'."
           nix-current-profile))

(provide 'nix-profiles)

;;; nix-profiles.el ends here
