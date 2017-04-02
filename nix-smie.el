;;; nix-mode+.el --- Major mode to edit Nix files

;; Copyright (C) 2014 Jenny Cassou

;; Author: Jenny Cassou <damien.cassou@gmail.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode to edit Nix files

;;; Code:
(require 'smie)

(defvar nix+-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (expr (exprfunction))
      (exprfunction ("{" formals "}" ":" exprfunction)
                    (exprassert))
      (exprassert ("assert" expr ";" exprassert)
                  (exprif))
      (exprif ("if" expr "then" expr "else" expr)
              (exprop))
      (exprop ("!" exprop)
              (exprop "==" exprop)
              (exprop "!=" exprop)
              (exprop "&&" exprop)
              (exprop "||" exprop)
              (exprop "->" exprop)
              (exprop "//" exprop)
              (exprop "~" exprop)
              (exprop "?" id)
              (exprapp))
      (exprapp (exprapp "." exprselect)
               (exprselect))
      (exprselect (exprsimple))
      (exprsimple (id)
                  ("true")
                  ("false")
                  ("null")
                  ("(" expr ")")
                  ("{" binds "}")
                  ("let" "{" binds "}")
                  ("rec" "{" binds "}")
                  ("[" exprselects "]"))
      (binds (binds ";" binds)
             (bind))
      (exprselects (expreselect))
      (bind (id "=" expr)
            ("inherit" exprp))
      (exprp ("(" expr ")"))
      (formals (formals "," formals)
               (formal))
      (formal (id)
              (id "?" expr)))
    '((assoc ";"))
    '((assoc ","))
    '((assoc "==")
      (assoc "!=")
      (assoc "&&")
      (assoc "||")
      (assoc "->")
      (assoc "//")
      (assoc "~")
      (assoc "?")
      (assoc "!")))))

(defvar nix+-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for Nix mode.")

;;;###autoload
(define-derived-mode nix-mode+ prog-mode "Nix"
  "Major mode for editing Nix expressions.

The following commands may be useful:

  '\\[newline-and-indent]'
    Insert a newline and move the cursor to align with the previous
    non-empty line.

  '\\[fill-paragraph]'
    Refill a paragraph so that all lines are at most `fill-column'
    lines long.  This should do the right thing for comments beginning
    with `#'.  However, this command doesn't work properly yet if the
    comment is adjacent to code (i.e., no intervening empty lines).
    In that case, select the text to be refilled and use
    `\\[fill-region]' instead.

The hook `nix-mode-hook' is run when Nix mode is started.

\\{nix-mode-map}
"
  (set-syntax-table nix+-syntax-table)
  (smie-setup nix+-smie-grammar #'ignore))

(provide 'nix-mode+)

;;; nix-mode+.el ends here
