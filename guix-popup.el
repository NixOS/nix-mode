;;; guix-popup.el --- Popup windows library

;; Copyright © 2015 Alex Kost <alezost@gmail.com>

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

;; This file provides `guix-define-popup' macro which is just an alias
;; to `magit-define-popup'.  According to the manual:
;;
;;   (info "(magit-popup) Defining prefix and suffix commands")
;;
;; `magit-popup' library will eventually be superseded by a more general
;; library.

;;; Code:

(require 'magit-popup)

(defalias 'guix-define-popup 'magit-define-popup)

(defvar guix-popup-font-lock-keywords
  (eval-when-compile
    `((,(rx "("
            (group "guix-define-popup")
            symbol-end
            (zero-or-more blank)
            (zero-or-one
             (group (one-or-more (or (syntax word) (syntax symbol))))))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode guix-popup-font-lock-keywords)

(provide 'guix-popup)

;;; guix-popup.el ends here
