;;; guix-prettify.el --- Prettify Guix store file names

;; Copyright © 2014, 2015, 2017 Alex Kost <alezost@gmail.com>

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

;; This package provides minor-mode for prettifying Guix store file
;; names — i.e., after enabling `guix-prettify-mode',
;; '/gnu/store/72f54nfp6g1hz873w8z3gfcah0h4nl9p-foo-0.1' names will be
;; replaced with '/gnu/store/…-foo-0.1' in the current buffer.  There is
;; also `global-guix-prettify-mode' for global prettifying.

;; To install, add the following to your emacs init file:
;;
;;   (add-to-list 'load-path "/path/to/dir-with-guix-prettify")
;;   (autoload 'guix-prettify-mode "guix-prettify" nil t)
;;   (autoload 'global-guix-prettify-mode "guix-prettify" nil t)

;; If you want to enable/disable composition after "M-x font-lock-mode",
;; use the following setting:
;;
;;   (setq font-lock-extra-managed-props
;;         (cons 'composition font-lock-extra-managed-props))

;; Credits:
;;
;; Thanks to Ludovic Courtès for the idea of this package.
;;
;; Thanks to the authors of `prettify-symbols-mode' (part of Emacs 24.4)
;; and "pretty-symbols.el" <http://github.com/drothlis/pretty-symbols>
;; for the code.  It helped to write this package.

;;; Code:

(require 'guix nil t)
(require 'guix-utils)

(defgroup guix-prettify nil
  "Prettify Guix store file names."
  :prefix "guix-prettify-"
  :group 'guix
  :group 'font-lock
  :group 'convenience)

(defcustom guix-prettify-char ?…
  "Character used for prettifying."
  :type 'character
  :group 'guix-prettify)

(defcustom guix-prettify-decompose-force nil
  "If non-nil, remove any composition.

By default, after disabling `guix-prettify-mode',
compositions (prettifying names with `guix-prettify-char') are
removed only from strings matching `guix-prettify-regexp', so
that compositions created by other modes are left untouched.

Set this variable to non-nil, if you want to remove any
composition unconditionally (like `prettify-symbols-mode' does).
Most likely it will do no harm and will make the process of
disabling `guix-prettify-mode' a little faster."
  :type 'boolean
  :group 'guix-prettify)

(defcustom guix-prettify-regexp
  ;; The following file names / URLs should be abbreviated:

  ;; /gnu/store/aiywpm2w299pk1ps96a8d8qwnwkzfr2g-foo-0.1
  ;; /nix/store/inb6pfvfm2vqpn9wlyrivj3iyx7k2pv6-foo-0.1
  ;; http://hydra.gnu.org/nar/hrr424q661d9wdpkr48gyk5a9w8nrlcr-foo-0.1
  ;; http://hydra.gnu.org/log/fjbx25bap58k3mywzpmc8w9fjdydxqv8-foo-0.1
  ;; https://bayfront.guixsd.org/nar/gzip/m4ccn9nzlsbvlj36w45555pq98spy007-foo-0.1

  (rx "/" (or "store" "log" (and "nar" (zero-or-one "/gzip")))
      ;; Hash-parts do not include "e", "o", "u" and "t".  See base32Chars
      ;; at <https://github.com/NixOS/nix/blob/master/src/libutil/hash.cc>
      "/" (group (= 32 (any "0-9" "a-d" "f-n" "p-s" "v-z"))))
  "Regexp matching file names for prettifying.

Disable `guix-prettify-mode' before modifying this variable and
make sure to modify `guix-prettify-regexp-group' if needed.

Example of a \"deeper\" prettifying:

  (setq guix-prettify-regexp \"store/[[:alnum:]]\\\\\\={32\\\\}\"
        guix-prettify-regexp-group 0)

This will transform
'/gnu/store/72f54nfp6g1hz873w8z3gfcah0h4nl9p-foo-0.1' into
'/gnu/…-foo-0.1'"
  :type 'regexp
  :group 'guix-prettify)

(defcustom guix-prettify-regexp-group 1
  "Regexp group in `guix-prettify-regexp' for prettifying."
  :type 'integer
  :group 'guix-prettify)

(defvar guix-prettify-special-modes
  '(ibuffer-mode)
  "List of special modes that support font-locking.

By default, \\[global-guix-prettify-mode] enables prettifying in
all buffers except the ones where `font-lock-defaults' is
nil (see Info node `(elisp) Font Lock Basics'), because it may
break the existing highlighting.

Modes from this list and all derived modes are exceptions
\(`global-guix-prettify-mode' enables prettifying there).")

(defun guix-prettify-compose ()
  "Compose matching region in the current buffer."
  (let ((beg (match-beginning guix-prettify-regexp-group))
        (end (match-end       guix-prettify-regexp-group)))
    (compose-region beg end guix-prettify-char 'decompose-region))
  ;; Return nil because we're not adding any face property.
  nil)

(defun guix-prettify-decompose-buffer ()
  "Remove file names compositions from the current buffer."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (if guix-prettify-decompose-force
          (remove-text-properties (point-min)
                                  (point-max)
                                  '(composition nil))
        (guix-while-search guix-prettify-regexp
          (remove-text-properties
           (match-beginning guix-prettify-regexp-group)
           (match-end       guix-prettify-regexp-group)
           '(composition nil)))))))

;;;###autoload
(define-minor-mode guix-prettify-mode
  "Toggle Guix Prettify mode.

With a prefix argument ARG, enable Guix Prettify mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Guix Prettify mode is enabled, hash parts of the Guix store
file names (see `guix-prettify-regexp') are displayed as
`guix-prettify-char' character, i.e.:

  /gnu/store/…-foo-0.1  instead of:
  /gnu/store/72f54nfp6g1hz873w8z3gfcah0h4nl9p-foo-0.1

This mode can be enabled programmatically using hooks:

  (add-hook 'shell-mode-hook 'guix-prettify-mode)

It is possible to enable the mode in any buffer, however not any
buffer's highlighting may survive after adding new elements to
`font-lock-keywords' (see `guix-prettify-special-modes' for
details).

Also you can use `global-guix-prettify-mode' to enable Guix
Prettify mode for all modes that support font-locking."
  :init-value nil
  :lighter " …"
  (let ((keywords `((,guix-prettify-regexp
                     (,guix-prettify-regexp-group
                      (guix-prettify-compose))))))
    (if guix-prettify-mode
        ;; Turn on.
        (font-lock-add-keywords nil keywords)
      ;; Turn off.
      (font-lock-remove-keywords nil keywords)
      (guix-prettify-decompose-buffer))
    (guix-font-lock-flush)))

(defun guix-prettify-supported-p ()
  "Return non-nil, if the mode can be harmlessly enabled in current buffer."
  (or font-lock-defaults
      (apply #'derived-mode-p guix-prettify-special-modes)))

(defun guix-prettify-turn-on ()
  "Enable `guix-prettify-mode' in the current buffer if needed.
See `guix-prettify-special-modes' for details."
  (and (not guix-prettify-mode)
       (guix-prettify-supported-p)
       (guix-prettify-mode)))

;;;###autoload
(define-globalized-minor-mode global-guix-prettify-mode
  guix-prettify-mode guix-prettify-turn-on)

;;;###autoload
(defalias 'guix-prettify-global-mode 'global-guix-prettify-mode)

(provide 'guix-prettify)

;;; guix-prettify.el ends here
