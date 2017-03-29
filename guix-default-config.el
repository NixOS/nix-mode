;;; guix-default-config.el --- Default values for configuration variables

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

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

;; This file is a "companion" of "guix-build-config.el".  It is used
;; when Emacs-Guix wasn't build with .configure/make, and provides some
;; sensible defaults for the according `guix-config-...' variables.  See
;; also "guix-config.el".

;;; Code:

(defconst guix-config-name "Emacs-Guix"
  "Emacs-Guix full name.")

(declare-function lm-header "lisp-mnt" (header))

(defconst guix-config-version
  ;; Find version in "guix.el".
  (let ((guix.el (expand-file-name "guix.el"
                                   (file-name-directory load-file-name))))
    (or (and (file-exists-p guix.el)
             (require 'lisp-mnt nil t)
             (with-temp-buffer
               (insert-file-contents-literally guix.el)
               (lm-header "version")))
        "<unknown version>"))
  "Emacs-Guix version.")

(defconst guix-config-image-directory nil
  "Directory with image files for Emacs-Guix.")

(defconst guix-config-scheme-directory nil
  "Directory with Scheme files for Emacs-Guix.")

(defconst guix-config-scheme-compiled-directory nil
  "Directory with compiled Scheme (*.go) files for Emacs-Guix.")

(defconst guix-config-guix-scheme-directory nil
  "Directory with Guix modules.")

(defconst guix-config-guix-scheme-compiled-directory nil
  "Directory with Guix compiled (*.go) files.")

(defconst guix-config-guile-program nil
  "Name of the 'guile' executable defined at configure time.")

(provide 'guix-default-config)

;;; guix-default-config.el ends here
