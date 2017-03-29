;;; guix-about.el --- Various info about Guix and Emacs-Guix  -*- lexical-binding: t -*-

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

;; This file provides the code to display various info about Guix (e.g., its
;; version) and Emacs-Guix.

;;; Code:

(require 'bui)
(require 'guix nil t)
(require 'guix-help)
(require 'guix-utils)
(require 'guix-config)

(declare-function guix-eval-read "guix-repl" (str))

;;;###autoload
(defun guix-version ()
  "Display Emacs-Guix and Guix versions in the echo area."
  (interactive)
  (require 'guix-repl)
  (message "%s %s\n%s %s"
           (guix-eval-read "(@ (guix config) %guix-package-name)")
           (guix-eval-read "(@ (guix config) %guix-version)")
           guix-config-name
           guix-config-version))


;;; "About" buffer

(defcustom guix-about-buffer-name "*Guix About*"
  "Buffer name for '\\[guix-about]'."
  :type 'string
  :group 'guix)

(defvar guix-about-specifications
  `("GNU Guix:   "
    :link ("https://www.gnu.org/software/guix/"
           ,(lambda (button)
              (browse-url (button-label button))))
    "\nEmacs-Guix: "
    :link ("https://github.com/alezost/guix.el"
           ,(lambda (button)
              (browse-url (button-label button))))
    "\n\n"
    :link ("GNU Guix Manual"
           ,(lambda (_button) (info "(guix)")))
    "\n"
    :link ("Emacs Guix Manual"
           ,(lambda (_button) (info "(emacs-guix)")))
    "\n"
    "\nAvailable commands: "
    :link ("M-x guix-help"
           ,(lambda (_button) (guix-help)))
    "\nGuix and Emacs-Guix versions: "
    :link ("M-x guix-version"
           ,(lambda (_button) (guix-version)))

    "\n")
  "Text to show with '\\[guix-about]' command.
This is not really a text, it is a list of arguments passed to
`fancy-splash-insert'.")

(defun guix-logo-file ()
  "Return the file name of Guix(SD) logo image.
Return nil, if the image cannot be found."
  (when guix-image-directory
    (expand-file-name (if (guix-guixsd?)
                          "guixsd-logo.svg"
                        "guix-logo.svg")
                      guix-image-directory)))

(defun guix-insert-logo ()
  "Insert Guix(SD) logo into the current buffer."
  (when (display-images-p)
    (let* ((file  (guix-logo-file))
           (image (and file (create-image file))))
      (when image
        (let ((width (car (image-size image))))
          (when (> (window-width) width)
            ;; Center the image in the window.
            (insert (propertize
                     " " 'display
                     `(space :align-to (+ center (-0.5 . ,image)))))
            (insert-image image)
            (bui-newline)))))))

(defun guix-about-insert-content ()
  "Insert Emacs-Guix 'about' info into the current buffer."
  (guix-insert-logo)
  (apply #'fancy-splash-insert guix-about-specifications)
  (goto-char (point-min))
  (forward-line 3))

(defun guix-about-show ()
  "Display 'About' buffer with fancy Guix logo if available.
Unlike `guix-about', this command always recreates
`guix-about-buffer-name' buffer."
  (interactive)
  (guix-help-display-buffer guix-about-buffer-name
                            #'guix-about-insert-content))

;;;###autoload
(defun guix-about ()
  "Display 'About' buffer with fancy Guix logo if available.
Switch to `guix-about-buffer-name' buffer if it already exists."
  (interactive)
  (guix-switch-to-buffer-or-funcall
   guix-about-buffer-name #'guix-about-show))

(provide 'guix-about)

;;; guix-about.el ends here
