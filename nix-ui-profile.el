;;; nix-ui-profile.el --- Interface for displaying profiles  -*- lexical-binding: t -*-

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

;; This file provides a 'list' interface for displaying Guix profiles
;; with `nix-profiles' command.
;;
;; `nix-profiles' variable controls what profiles are displayed.

;;; Code:

(require 'dash)
(require 'bui)
(require 'nix)
(require 'nix-profiles)
(require 'nix-utils)

(nix-define-groups profile)

(defcustom nix-profiles
  (-filter #'file-exists-p
           (list nix-user-profile
                 nix-system-profile))
  "List of profiles displayed by '\\[nix-profiles]' command."
  :type '(repeat file)
  :group 'nix-profile)

(defun nix-profile->entry (profile)
  "Return 'nix-profile' entry by PROFILE file-name."
  `((id                    . ,profile)
    (profile               . ,profile)
    (current               . ,(nix-current-profile? profile))
    (number-of-packages    . ,(nix-profile-number-of-packages
                               profile))
    (number-of-generations . ,(nix-profile-number-of-generations
                               profile))))

(defun nix-profile-get-entries ()
  "Return 'nix-profile' entries."
  (mapcar #'nix-profile->entry nix-profiles))


;;; Profile 'list'

(bui-define-interface nix-profile list
  :mode-name "Profile-List"
  :buffer-name "*Nix Profiles*"
  :get-entries-function 'nix-profile-get-entries
  :format '((current nix-profile-list-get-current 10 t)
            (profile bui-list-get-file-name 40 t)
            (number-of-packages nil 11 bui-list-sort-numerically-2
                                :right-align t)
            (number-of-generations nil 14 bui-list-sort-numerically-3
                                   :right-align t))
  :titles '((number-of-packages    . "Packages")
            (number-of-generations . "Generations"))
  :hint 'nix-profile-list-hint
  :sort-key '(profile))

(defun nix-profile-list-hint ()
  (bui-format-hints
   nix-profile-list-default-hint
   bui-list-sort-hint
   bui-common-hint))

(defun nix-profile-list-current-profile ()
  "Return file name of the current profile."
  ;; (bui-entry-value (bui-list-current-entry) 'profile)
  ;; Just get the ID, as currently ID is the profile file name.
  (bui-list-current-id))

(defun nix-profile-list-get-current (value &optional _)
  "Return string from VALUE showing whether this profile is current."
  (if value "(current)" ""))

(defun nix-profile-list-set-current ()
  "Set `nix-current-profile' to the profile on the current line."
  (interactive)
  (nix-set-current-profile (nix-profile-list-current-profile))
  ;; Now updating "Current" column is needed.  It can be done simply by
  ;; reverting the buffer, but it should be more effective to reset
  ;; 'current' parameter for all entries and to redisplay the buffer
  ;; instead.
  (let* ((current-id  (bui-list-current-id))
         (new-entries (mapcar
                       (lambda (entry)
                         (let ((id (bui-entry-id entry)))
                           (cons `(current . ,(equal id current-id))
                                 (--remove-first (eq (car it) 'current)
                                                 entry))))
                       (bui-current-entries))))
    (setf (bui-item-entries bui-item)
          new-entries))
  (bui-redisplay))


;;; Interactive commands

(defun nix-profiles-show ()
  "Display Nix profiles.
Unlike `nix-profiles', this command always recreates
`nix-profile-list-buffer-name' buffer."
  (interactive)
  (bui-list-get-display-entries 'nix-profile))

;;;###autoload
(defun nix-profiles ()
  "Display Nix profiles.
Switch to the `nix-profile-list-buffer-name' buffer if it
already exists.

Modify `nix-profiles' variable to add more profiles."
  (interactive)
  (nix-switch-to-buffer-or-funcall
   nix-profile-list-buffer-name #'nix-profiles-show 'message))

(provide 'nix-ui-profile)

;;; nix-ui-profile.el ends here
