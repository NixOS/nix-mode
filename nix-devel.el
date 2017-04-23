;;; nix-devel.el --- Development tools  -*- lexical-binding: t -*-

;; Copyright © 2015–2017 Alex Kost <alezost@gmail.com>

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

;; This file provides `nix-devel-mode' (minor mode for `scheme-mode'
;; buffers) that provides highlighting and indentation rules for Guix
;; Guile code, as well as some tools to work with Guix (or even an
;; arbitrary Guile code) with Geiser.

;;; Code:

(require 'lisp-mode)
(require 'bui-utils)
(require 'guix nil t)
(require 'nix-utils)
(require 'nix-guile)
(require 'nix-geiser)
(require 'nix-misc)

(defgroup nix-devel nil
  "Settings for Guix development utils."
  :group 'guix)

(defgroup nix-devel-faces nil
  "Faces for `nix-devel-mode'."
  :group 'nix-devel
  :group 'nix-faces)

(defface nix-devel-modify-phases-keyword
  '((t :inherit font-lock-preprocessor-face))
  "Face for a `modify-phases' keyword ('delete', 'replace', etc.)."
  :group 'nix-devel-faces)

(defface nix-devel-gexp-symbol
  '((t :inherit font-lock-keyword-face))
  "Face for gexp symbols ('#~', '#$', etc.).
See Info node `(guix) G-Expressions'."
  :group 'nix-devel-faces)

(defun nix-devel-use-modules (&rest modules)
  "Use guile MODULES."
  (apply #'nix-geiser-call "use-modules" modules))

(defun nix-devel-use-module (&optional module)
  "Use guile MODULE in the current Geiser REPL.
MODULE is a string with the module name - e.g., \"(ice-9 match)\".
Interactively, use the module defined by the current scheme file."
  (interactive (list (nix-guile-current-module)))
  (nix-devel-use-modules module)
  (message "Using %s module." module))

(defun nix-devel-copy-module-as-kill ()
  "Put the name of the current guile module into `kill-ring'."
  (interactive)
  (bui-copy-as-kill (nix-guile-current-module)))

(defun nix-devel-setup-repl (&optional repl)
  "Setup REPL for using `nix-devel-...' commands."
  (nix-devel-use-modules "(guix monad-repl)"
                          "(guix scripts)"
                          "(guix store)"
                          "(guix ui)")
  ;; Without this workaround, the warning/build output disappears.  See
  ;; <https://github.com/jaor/geiser/issues/83> for details.
  (nix-geiser-eval-in-repl-synchronously
   "(begin
      (nix-warning-port (current-warning-port))
      (current-build-output-port (current-error-port)))"
   repl 'no-history 'no-display))

(defvar nix-devel-repl-processes nil
  "List of REPL processes configured by `nix-devel-setup-repl'.")

(defun nix-devel-setup-repl-maybe (&optional repl)
  "Setup (if needed) REPL for using `nix-devel-...' commands."
  (let ((process (get-buffer-process (or repl (nix-geiser-repl)))))
    (when (and process
               (not (memq process nix-devel-repl-processes)))
      (nix-devel-setup-repl repl)
      (push process nix-devel-repl-processes))))

(defmacro nix-devel-with-definition (def-var &rest body)
  "Run BODY with the current guile definition bound to DEF-VAR.
Bind DEF-VAR variable to the name of the current top-level
definition, setup the current REPL, use the current module, and
run BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,def-var (nix-guile-current-definition)))
     (nix-devel-setup-repl-maybe)
     (nix-devel-use-modules (nix-guile-current-module))
     ,@body))

(defun nix-devel-build-package-definition ()
  "Build a package defined by the current top-level variable definition."
  (interactive)
  (nix-devel-with-definition def
    (when (or (not nix-operation-confirm)
              (nix-operation-prompt (format "Build '%s'?" def)))
      (nix-geiser-eval-in-repl
       (concat ",run-in-store "
               (nix-guile-make-call-expression
                "build-package" def
                "#:use-substitutes?" (nix-guile-boolean
                                      nix-use-substitutes)
                "#:dry-run?" (nix-guile-boolean nix-dry-run)))))))

(defun nix-devel-build-package-source ()
  "Build the source of the current package definition."
  (interactive)
  (nix-devel-with-definition def
    (when (or (not nix-operation-confirm)
              (nix-operation-prompt
               (format "Build '%s' package source?" def)))
      (nix-geiser-eval-in-repl
       (concat ",run-in-store "
               (nix-guile-make-call-expression
                "build-package-source" def
                "#:use-substitutes?" (nix-guile-boolean
                                      nix-use-substitutes)
                "#:dry-run?" (nix-guile-boolean nix-dry-run)))))))

(defun nix-devel-download-package-source ()
  "Download the source of the current package.
Use this function to compute SHA256 hash of the package source."
  (interactive)
  (nix-devel-with-definition def
    (nix-devel-use-modules "(guix packages)"
                            "(guix scripts download)")
    (when (or (not nix-operation-confirm)
              (y-or-n-p (format "Download '%s' package source?" def)))
      (nix-geiser-eval-in-repl
       (format "(nix-download (origin-uri (package-source %s)))"
               def)))))

(defun nix-devel-lint-package ()
  "Check the current package.
See Info node `(guix) Invoking guix lint' for details."
  (interactive)
  (nix-devel-with-definition def
                              (nix-devel-use-modules "(guix scripts lint)")
                              (when (or (not nix-operation-confirm)
                                        (y-or-n-p (format "Lint '%s' package?" def)))
                                (nix-geiser-eval-in-repl
                                 (format "(run-checkers %s)" def)))))

;;; Font-lock

(defvar nix-devel-modify-phases-keyword-regexp
  (rx (or "delete" "replace" "add-before" "add-after"))
  "Regexp for a 'modify-phases' keyword.")

(defun nix-devel-modify-phases-font-lock-matcher (limit)
  "Find a 'modify-phases' keyword.
This function is used as a MATCHER for `font-lock-keywords'."
  (ignore-errors
    (down-list)
    (or (re-search-forward nix-devel-modify-phases-keyword-regexp
                           limit t)
        (set-match-data nil))
    (up-list)
    t))

(defun nix-devel-modify-phases-font-lock-pre ()
  "Skip the next sexp, and return the end point of the current list.
This function is used as a PRE-MATCH-FORM for `font-lock-keywords'
to find 'modify-phases' keywords."
  (let ((in-comment? (nth 4 (syntax-ppss))))
    ;; If 'modify-phases' is commented, do not try to search for its
    ;; keywords.
    (unless in-comment?
      (ignore-errors (forward-sexp))
      (save-excursion (up-list) (point)))))

(defconst nix-devel-keywords
  '("call-with-compressed-output-port"
    "call-with-container"
    "call-with-decompressed-port"
    "call-with-derivation-narinfo"
    "call-with-derivation-substitute"
    "call-with-error-handling"
    "call-with-gzip-input-port"
    "call-with-gzip-output-port"
    "call-with-temporary-directory"
    "call-with-temporary-output-file"
    "define-enumerate-type"
    "define-gexp-compiler"
    "define-lift"
    "define-monad"
    "define-operation"
    "define-record-type*"
    "emacs-substitute-sexps"
    "emacs-substitute-variables"
    "mbegin"
    "mlambda"
    "mlambdaq"
    "mlet"
    "mlet*"
    "modify-services"
    "munless"
    "mwhen"
    "run-with-state"
    "run-with-store"
    "signature-case"
    "substitute*"
    "substitute-keyword-arguments"
    "test-assertm"
    "use-package-modules"
    "use-service-modules"
    "use-system-modules"
    "with-atomic-file-output"
    "with-atomic-file-replacement"
    "with-derivation-narinfo"
    "with-derivation-substitute"
    "with-directory-excursion"
    "with-error-handling"
    "with-imported-modules"
    "with-monad"
    "with-mutex"
    "with-store"))

(defvar nix-devel-font-lock-keywords
  `((,(rx (or "#~" "#$" "#$@" "#+" "#+@")) .
     'nix-devel-gexp-symbol)
    (,(nix-guile-keyword-regexp (regexp-opt nix-devel-keywords))
     (1 'font-lock-keyword-face))
    (,(nix-guile-keyword-regexp "modify-phases")
     (1 'font-lock-keyword-face)
     (nix-devel-modify-phases-font-lock-matcher
      (nix-devel-modify-phases-font-lock-pre)
      nil
      (0 'nix-devel-modify-phases-keyword nil t))))
  "A list of `font-lock-keywords' for `nix-devel-mode'.")

;;; Indentation

(defmacro nix-devel-scheme-indent (&rest rules)
  "Set `scheme-indent-function' according to RULES.
Each rule should have a form (SYMBOL VALUE).  See `put' for details."
  (declare (indent 0))
  `(progn
     ,@(mapcar (lambda (rule)
                 `(put ',(car rule) 'scheme-indent-function ,(cadr rule)))
               rules)))

(defun nix-devel-indent-package (state indent-point normal-indent)
  "Indentation rule for 'package' form."
  (let* ((package-eol (line-end-position))
         (count (if (and (ignore-errors (down-list) t)
                         (< (point) package-eol)
                         (looking-at "inherit\\>"))
                    1
                  0)))
    (lisp-indent-specform count state indent-point normal-indent)))

(defun nix-devel-indent-modify-phases-keyword (count)
  "Return indentation function for 'modify-phases' keywords."
  (lambda (state indent-point normal-indent)
    (when (ignore-errors
            (goto-char (nth 1 state))   ; start of keyword sexp
            (backward-up-list)
            (looking-at "(modify-phases\\>"))
      (lisp-indent-specform count state indent-point normal-indent))))

(defalias 'nix-devel-indent-modify-phases-keyword-1
  (nix-devel-indent-modify-phases-keyword 1))
(defalias 'nix-devel-indent-modify-phases-keyword-2
  (nix-devel-indent-modify-phases-keyword 2))

(nix-devel-scheme-indent
 (bag 0)
 (build-system 0)
 (call-with-compressed-output-port 2)
 (call-with-container 1)
 (call-with-gzip-input-port 1)
 (call-with-gzip-output-port 1)
 (call-with-decompressed-port 2)
 (call-with-error-handling 0)
 (container-excursion 1)
 (emacs-batch-edit-file 1)
 (emacs-batch-eval 0)
 (emacs-substitute-sexps 1)
 (emacs-substitute-variables 1)
 (file-system 0)
 (graft 0)
 (manifest-entry 0)
 (manifest-pattern 0)
 (mbegin 1)
 (mlambda 1)
 (mlambdaq 1)
 (mlet 2)
 (mlet* 2)
 (modify-phases 1)
 (modify-services 1)
 (munless 1)
 (mwhen 1)
 (operating-system 0)
 (origin 0)
 (package 'nix-devel-indent-package)
 (run-with-state 1)
 (run-with-store 1)
 (signature-case 1)
 (substitute* 1)
 (substitute-keyword-arguments 1)
 (test-assertm 1)
 (with-atomic-file-output 1)
 (with-derivation-narinfo 1)
 (with-derivation-substitute 2)
 (with-directory-excursion 1)
 (with-error-handling 0)
 (with-imported-modules 1)
 (with-monad 1)
 (with-mutex 1)
 (with-store 1)
 (wrap-program 1)

 ;; 'modify-phases' keywords:
 (replace    'nix-devel-indent-modify-phases-keyword-1)
 (add-after  'nix-devel-indent-modify-phases-keyword-2)
 (add-before 'nix-devel-indent-modify-phases-keyword-2))

(defvar nix-devel-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'nix-devel-build-package-definition)
    (define-key map (kbd "s") 'nix-devel-build-package-source)
    (define-key map (kbd "d") 'nix-devel-download-package-source)
    (define-key map (kbd "l") 'nix-devel-lint-package)
    (define-key map (kbd "k") 'nix-devel-copy-module-as-kill)
    (define-key map (kbd "u") 'nix-devel-use-module)
    map)
  "Keymap with subkeys for `nix-devel-mode-map'.")

(defvar nix-devel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") nix-devel-keys-map)
    map)
  "Keymap for `nix-devel-mode'.")

;;;###autoload
(define-minor-mode nix-devel-mode
  "Minor mode for `scheme-mode' buffers.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When Guix Devel mode is enabled, it highlights various Guix
keywords.  This mode can be enabled programmatically using hooks,
like this:

  (add-hook 'scheme-mode-hook 'nix-devel-mode)

\\{nix-devel-mode-map}"
  :init-value nil
  :lighter " Nix"
  :keymap nix-devel-mode-map
  (if nix-devel-mode
      (progn
        (setq-local font-lock-multiline t)
        (font-lock-add-keywords nil nix-devel-font-lock-keywords))
    (setq-local font-lock-multiline nil)
    (font-lock-remove-keywords nil nix-devel-font-lock-keywords))
  (nix-font-lock-flush))

(defvar nix-devel-emacs-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group "nix-devel-with-definition") symbol-end) . 1))))

(font-lock-add-keywords 'emacs-lisp-mode
                        nix-devel-emacs-font-lock-keywords)

(provide 'nix-devel)

;;; nix-devel.el ends here
