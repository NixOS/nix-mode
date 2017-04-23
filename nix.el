;;; help-vars.el --- Variables related to Guix --help output

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

;; This file provides regular expressions to parse various "guix
;; ... --help" outputs and lists of non-receivable items (system types,
;; hash formats, etc.).

;;; Code:

(defgroup nix nil
  "Interface for Nix package manager."
  :prefix "nix-"
  :group 'external')

(defgroup nix-faces nil
  "Nix faces."
  :group 'nix
  :group 'faces)

(defvar nix-system-types
  '("x86_64-linux" "i686-linux" "aarch64-linux" "x86_64-darwin")
  "List of supported systems.")

(provide 'nix)
;;; nix.el ends here
