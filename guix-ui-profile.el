;;; guix-ui-profile.el --- Interface for displaying profiles  -*- lexical-binding: t -*-

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
;; with `guix-profiles' command.
;;
;; `guix-profiles' variable controls what profiles are displayed.

;;; Code:

(require 'dash)
(require 'bui)
(require 'guix nil t)
(require 'guix-profiles)
(require 'guix-utils)

(guix-define-groups profile)

(defcustom guix-profiles
  (-filter #'file-exists-p
           (list guix-user-profile
                 guix-system-profile))
  "List of profiles displayed by '\\[guix-profiles]' command."
  :type '(repeat file)
  :group 'guix-profile)

(defun guix-profile->entry (profile)
  "Return 'guix-profile' entry by PROFILE file-name."
  `((id                    . ,profile)
    (profile               . ,profile)
    (current               . ,(guix-current-profile? profile))
    (number-of-packages    . ,(guix-profile-number-of-packages
                               profile))
    (number-of-generations . ,(guix-profile-number-of-generations
                               profile))))

(defun guix-profile-get-entries ()
  "Return 'guix-profile' entries."
  (mapcar #'guix-profile->entry guix-profiles))


;;; Profile 'list'

(bui-define-interface guix-profile list
  :mode-name "Profile-List"
  :buffer-name "*Guix Profiles*"
  :get-entries-function 'guix-profile-get-entries
  :format '((current guix-profile-list-get-current 10 t)
            (profile bui-list-get-file-name 40 t)
            (number-of-packages nil 11 bui-list-sort-numerically-2
                                :right-align t)
            (number-of-generations nil 14 bui-list-sort-numerically-3
                                   :right-align t))
  :titles '((number-of-packages    . "Packages")
            (number-of-generations . "Generations"))
  :hint 'guix-profile-list-hint
  :sort-key '(profile))

(let ((map guix-profile-list-mode-map))
  (define-key map (kbd "RET") 'guix-profile-list-show-packages)
  (define-key map (kbd "P") 'guix-profile-list-show-packages)
  (define-key map (kbd "G") 'guix-profile-list-show-generations)
  (define-key map (kbd "M") 'guix-profile-list-apply-manifest)
  (define-key map (kbd "c") 'guix-profile-list-set-current)
  ;; Unbind "i" as "Profile Info" interface is not defined.
  (define-key map (kbd "i") nil))

(defvar guix-profile-list-default-hint
  '(("\\[guix-profile-list-show-packages]") " show packages;\n"
    ("\\[guix-profile-list-show-generations]") " show generations;\n"
    ("\\[guix-profile-list-set-current]") " set current profile;\n"))

(defun guix-profile-list-hint ()
  (bui-format-hints
   guix-profile-list-default-hint
   bui-list-sort-hint
   bui-common-hint))

(defun guix-profile-list-current-profile ()
  "Return file name of the current profile."
  ;; (bui-entry-value (bui-list-current-entry) 'profile)
  ;; Just get the ID, as currently ID is the profile file name.
  (bui-list-current-id))

(declare-function guix-installed-packages "guix-ui-package" t)
(declare-function guix-generations "guix-ui-generation" t)
(declare-function guix-system-generations "guix-ui-system-generation" t)
(declare-function guix-apply-manifest "guix-misc" t)

(defun guix-profile-list-show-packages ()
  "Display packages installed in the current profile."
  (interactive)
  (guix-installed-packages (guix-package-profile
                            (guix-profile-list-current-profile))))

(defun guix-profile-list-show-generations ()
  "Display generations of the current profile."
  (interactive)
  (let ((profile (guix-profile-list-current-profile)))
    (if (guix-system-profile? profile)
        (guix-system-generations)
      (guix-generations (guix-generation-profile profile)))))

(defun guix-profile-list-apply-manifest (file)
  "Apply manifest from FILE to the current profile."
  (interactive
   (list (guix-read-file-name "File with manifest: ")))
  (guix-apply-manifest (guix-package-profile
                        (guix-profile-list-current-profile))
                       file (current-buffer)))

(defun guix-profile-list-get-current (value &optional _)
  "Return string from VALUE showing whether this profile is current."
  (if value "(current)" ""))

(defun guix-profile-list-set-current ()
  "Set `guix-current-profile' to the profile on the current line."
  (interactive)
  (guix-set-current-profile (guix-profile-list-current-profile))
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

(defun guix-profiles-show ()
  "Display Guix profiles.
Unlike `guix-profiles', this command always recreates
`guix-profile-list-buffer-name' buffer."
  (interactive)
  (bui-list-get-display-entries 'guix-profile))

;;;###autoload
(defun guix-profiles ()
  "Display Guix profiles.
Switch to the `guix-profile-list-buffer-name' buffer if it
already exists.

Modify `guix-profiles' variable to add more profiles."
  (interactive)
  (guix-switch-to-buffer-or-funcall
   guix-profile-list-buffer-name #'guix-profiles-show 'message))

(provide 'guix-ui-profile)

;;; guix-ui-profile.el ends here
