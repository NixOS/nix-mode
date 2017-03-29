;;; guix-help-vars.el --- Variables related to Guix --help output

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


;;; Regexps for parsing "guix ..." outputs

(defvar guix-help-parse-option-regexp
  (rx bol "  "
      (zero-or-one (group "-" (not (any "- ")))
                   ",")
      (one-or-more " ")
      (group "--" (one-or-more (or wordchar "-")))
      (group (zero-or-one "[")
             (zero-or-one "="))
      (zero-or-more (not space))
      (one-or-more space)
      (group (one-or-more any)))
  "Common regexp used to find command options.")

(defvar guix-help-parse-command-regexp
  (rx bol "   "
      (group wordchar (one-or-more (or wordchar "-"))))
  "Regexp used to find guix commands.
'Command' means any option not prefixed with '-'.  For example,
guix subcommand, system action, importer, etc.")

(defvar guix-help-parse-long-option-regexp
  (rx (or "  " ", ")
      (group "--" (one-or-more (or wordchar "-"))
             (zero-or-one "=")))
  "Regexp used to find long options.")

(defvar guix-help-parse-short-option-regexp
  (rx bol (one-or-more blank)
      "-" (group (not (any "- "))))
  "Regexp used to find short options.")

(defvar guix-help-parse-package-regexp
  (rx bol (group (one-or-more (not blank))))
  "Regexp used to find names of the packages.")

(defvar guix-help-parse-list-regexp
  (rx bol (zero-or-more blank) "- "
      (group (one-or-more (or wordchar "-"))))
  "Regexp used to find various lists (lint checkers, graph types).")

(defvar guix-help-parse-regexp-group 1
  "Parenthesized expression of regexps used to find commands and
options.")


;;; Non-receivable lists of system types, hash formats, etc.

(defvar guix-help-system-types
  '("x86_64-linux" "i686-linux" "armhf-linux" "mips64el-linux")
  "List of supported systems.")

(defvar guix-help-source-types
  '("package" "all" "transitive")
  "List of supported sources types.")

(defvar guix-help-hash-formats
  '("nix-base32" "base32" "base16" "hex" "hexadecimal")
  "List of supported hash formats.")

(defvar guix-help-refresh-subsets
  '("core" "non-core")
  "List of supported 'refresh' subsets.")

(defvar guix-help-key-policies
  '("interactive" "always" "never")
  "List of supported key download policies.")

(defvar guix-help-verify-options
  '("repair" "contents")
  "List of supported 'verify' options")

(defvar guix-help-elpa-archives
  '("gnu" "melpa" "melpa-stable")
  "List of supported ELPA archives.")

(provide 'guix-help-vars)

;;; guix-help-vars.el ends here
