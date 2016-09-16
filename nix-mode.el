;;; nix-mode.el --- Major mode for editing Nix expressions

;; Author: Eelco Dolstra
;; Maintainer: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/matthewbauer/nix-mode
;; Version: 1.1
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A major mode for editing Nix expressions (.nix files).  See the Nix manual
;; for more information available at https://nixos.org/nix/manual/.

;;; Code:

;; Emacs 24.2 compatability
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    `(set (make-local-variable ',var) ,val)))

(defun nix-syntax-match-antiquote (limit)
  "Find antiquote within a Nix expression up to LIMIT."
  (let ((pos (next-single-char-property-change (point) 'nix-syntax-antiquote
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((char (char-after pos)))
        (pcase char
          (`?$
           (forward-char 2))
          (`?}
           (forward-char 1)))
        (set-match-data (list pos (point)))
        t))))

(defconst nix-keywords
  '("if" "then"
    "else" "with"
    "let" "in"
    "rec" "inherit"
    "or"
    ))

(defconst nix-builtins
  '("builtins" "baseNameOf"
    "derivation" "dirOf"
    "false" "fetchTarball"
    "import" "isNull"
    "map" "removeAttrs"
    "toString" "true"))

(defconst nix-warning-keywords
  '("assert" "abort" "throw"))

(defconst nix-re-file-path
  "[a-zA-Z0-9._\\+-]*\\(/[a-zA-Z0-9._\\+-]+\\)+")

(defconst nix-re-url
  "[a-zA-Z][a-zA-Z0-9\\+-\\.]*:[a-zA-Z0-9%/\\?:@&=\\+\\$,_\\.!~\\*'-]+")

(defconst nix-re-bracket-path
  "<[a-zA-Z0-9._\\+-]+\\(/[a-zA-Z0-9._\\+-]+\\)*>")

(defconst nix-re-variable-assign
  "\\<\\([a-zA-Z_][a-zA-Z0-9_'\-\.]*\\)[ \t]*=")

(defconst nix-font-lock-keywords
  `(
    (,(regexp-opt nix-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt nix-warning-keywords 'symbols) . font-lock-warning-face)
    (,(regexp-opt nix-builtins 'symbols) . font-lock-builtin-face)
    (,nix-re-url . font-lock-constant-face)
    (,nix-re-file-path . font-lock-constant-face)
    (,nix-re-variable-assign 1 font-lock-variable-name-face)
    (,nix-re-bracket-path . font-lock-constant-face)
    (nix-syntax-match-antiquote 0 font-lock-preprocessor-face t))
  "Font lock keywords for nix.")

(defvar nix-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for Nix mode.")

(defun nix-syntax-propertize-escaped-antiquote ()
  "Set syntax properies for an escaped antiquote mark."
  nil)

(defun nix-syntax-propertize-multiline-string ()
  "Set syntax properies for multiline string delimiters."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (context (save-excursion (save-match-data (syntax-ppss start))))
         (string-type (nth 3 context)))
    (pcase string-type
      (`t
       ;; inside a multiline string
       ;; ending multi-line string delimiter
       (put-text-property (1- end) end
                          'syntax-table (string-to-syntax "|")))
      (`nil
       ;; beginning multi-line string delimiter
       (put-text-property start (1+ start)
                          'syntax-table (string-to-syntax "|"))))))

(defun nix-syntax-propertize-antiquote ()
  "Set syntax properties for an antiquote mark."
  (let* ((start (match-beginning 0)))
    (put-text-property start (1+ start)
                       'syntax-table (string-to-syntax "|"))
    (put-text-property start (+ start 2)
                       'nix-syntax-antiquote t)))

(defun nix-syntax-propertize-close-brace ()
  "Set syntax properties for close braces.
If a close brace `}' ends an antiquote, the next character begins a string."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (context (save-excursion (save-match-data (syntax-ppss start))))
         (open (nth 1 context)))
    (when open ;; a corresponding open-brace was found
      (let* ((antiquote (get-text-property open 'nix-syntax-antiquote)))
        (when antiquote
          (put-text-property (+ start 1) (+ start 2)
                             'syntax-table (string-to-syntax "|"))
          (put-text-property start (1+ start)
                             'nix-syntax-antiquote t))))))

(defun nix-syntax-propertize (start end)
  "Special syntax properties for Nix from START to END."
  ;; search for multi-line string delimiters
  (goto-char start)
  (remove-text-properties start end '(syntax-table nil nix-syntax-antiquote nil))
  (funcall
   (syntax-propertize-rules
    ("''\\${"
     (0 (ignore (nix-syntax-propertize-escaped-antiquote))))
    ("''"
     (0 (ignore (nix-syntax-propertize-multiline-string))))
    ("\\${"
     (0 (ignore (nix-syntax-propertize-antiquote))))
    ("}"
     (0 (ignore (nix-syntax-propertize-close-brace)))))
   start end))

(defun nix-indent-level-parens ()
  "Find indent level based on parens."
  (setq n 0)
  (save-excursion
    (setq p1 (point))
    (setq p2 (nth 1 (syntax-ppss)))

    (while p2
      (goto-char p2)
      (backward-char)
      (let ((l1 (line-number-at-pos p1))
	    (l2 (line-number-at-pos p2)))
	(if (not (eq l1 l2))
	    (setq n (+ n 1))))
      (setq p1 p2)
      (setq p2 (nth 1 (syntax-ppss)))))

  n)

(defun nix-indent-level-is-closing ()
  "Go forward from beginning of line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "[:space:]")

    (or ;; any of these should -1 indent level
     (looking-at ")")
     (looking-at "}")
     (looking-at "''")
     (looking-at ","))))

(defun nix-indent-level-is-hanging ()
  "Is hanging?"
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "[:space:]")
    (unless (or
    	     (looking-at "}")
	     (looking-at ")")
	     (looking-at "{"))

      (forward-line -1)
      (end-of-line)
      (skip-chars-backward "\n[:space:]")

      ;; skip through any comments in the way
      (while (nth 4 (syntax-ppss))
	(goto-char (nth 8 (syntax-ppss)))
	(skip-chars-backward "\n[:space:]"))

      ;; any of these should be ignored
      (unless (or
	       (looking-back "/*")
	       (looking-back ";")
	       (looking-back ":")
	       (looking-back "{")
	       (looking-back "(")
	       (looking-back ",")
	       (looking-back "let")
	       (looking-back "in"))
	t))))

(defun nix-indent-level ()
  "Get current indent level."
  (* tab-width (+
		(nix-indent-level-parens)
		(if (nix-indent-level-is-closing) -1
		  (if (nix-indent-level-is-hanging) 1 0)))))

(defun nix-indent-line ()
  "Indent current line in a Nix expression."
  (interactive)
  (cond

   ;; comment
   ((save-excursion
      (beginning-of-line)
      (nth 4 (syntax-ppss))) nil)

   ;; string
   ((save-excursion
      (beginning-of-line)
      (nth 3 (syntax-ppss))) nil)

   ;; else
   (t
    (indent-line-to (nix-indent-level)))))

(defun nix-visit-file ()
  "Go to file under cursor."
  (interactive)
  (save-excursion
    (forward-whitespace -1)
    (skip-chars-forward " \t")
    (if (looking-at nix-re-file-path)
	(find-file (match-string-no-properties 0)))))

(defcustom nix-nixfmt-bin "nixfmt"
  "Path to nixfmt executable."
  :group 'nix
  :type 'string)

(defun nix--format-call (buf)
  "Format BUF using nixfmt."
  (with-current-buffer (get-buffer-create "*nixfmt*")
    (erase-buffer)
    (insert-buffer-substring buf)
    (if (zerop (call-process-region (point-min) (point-max) nix-nixfmt-bin t t nil))
        (progn
          (if (not (string= (buffer-string) (with-current-buffer buf (buffer-string))))
              (copy-to-buffer buf (point-min) (point-max)))
          (kill-buffer))
      (error "Nixfmt failed, see *nixfmt* buffer for details"))))

(defun nix-format-buffer ()
  "Format the current buffer using nixfmt."
  (interactive)
  (unless (executable-find nix-nixfmt-bin)
    (error "Could not locate executable \"%s\"" nix-nixfmt-bin))
  (nix--format-call (current-buffer))
  (message "Formatted buffer with rustfmt."))

(defvar nix-mode-menu (make-sparse-keymap "Nix")
  "Menu for Nix mode.")

(defvar nix-mode-map (make-sparse-keymap)
  "Local keymap used for Nix mode.")

(defun nix-create-keymap ()
  "Create the keymap associated with the Nix mode."
  (define-key nix-mode-map "\C-c\C-g" 'nix-visit-file)
  (define-key nix-mode-map "\C-c\C-f" 'nix-format-buffer))

(defun nix-create-menu ()
  "Create the Nix menu as shown in the menu bar."
  (let ((m '("Nix"
	     ["Goto file" nix-visit-file t]
	     ["Format buffer" nix-format-buffer t])
	   ))
    (easy-menu-define ada-mode-menu nix-mode-map "Menu keymap for Nix mode" m)))

(nix-create-keymap)
(nix-create-menu)

;;;###autoload
(define-derived-mode nix-mode prog-mode "Nix"
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
  (set-syntax-table nix-mode-syntax-table)

  ;; Disable hard tabs and set tab to 2 spaces
  ;; Recommended by nixpkgs manual: https://nixos.org/nixpkgs/manual/#sec-syntax
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)

  ;; Font lock support.
  (setq-local font-lock-defaults '(nix-font-lock-keywords nil nil nil nil))

  ;; Special syntax properties for Nix
  (setq-local syntax-propertize-function 'nix-syntax-propertize)

  ;; Look at text properties when parsing
  (setq-local parse-sexp-lookup-properties t)

  ;; Automatic indentation [C-j]
  (setq-local indent-line-function 'nix-indent-line)

  ;; Indenting of comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(^\\|\\s-\\);?#+ *")
  (setq-local comment-multi-line t)

  ;; Filling of comments
  (setq-local adaptive-fill-mode t)
  (setq-local paragraph-start "[ \t]*\\(#+[ \t]*\\)?$")
  (setq-local paragraph-separate paragraph-start)

  (easy-menu-add nix-mode-menu nix-mode-map))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode)))

(defun nix-mode-reload ()
  "Reload Nix mode."
  (interactive)
  (unload-feature 'nix-mode)
  (require 'nix-mode)
  (nix-mode))

(provide 'nix-mode)

;;; nix-mode.el ends here
