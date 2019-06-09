;;; nix-mode.el --- Major mode for editing .nix files -*- lexical-binding: t -*-

;; Maintainer: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Version: 1.2.1
;; Keywords: nix, languages, tools, unix
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A major mode for editing Nix expressions (.nix files).  See the Nix manual
;; for more information available at https://nixos.org/nix/manual/.

;;; Code:

(require 'nix)
(require 'nix-format)
(require 'nix-shebang)
(require 'nix-shell)
(require 'nix-repl)
(require 'smie)
(require 'ffap)
(eval-when-compile (require 'subr-x))

(defgroup nix-mode nil
  "Nix mode customizations"
  :group 'nix)

(defcustom nix-indent-function 'indent-relative
  "The function to use to indent.

Valid functions for this are:

- ‘indent-relative’
- smie-indent-line (buggy)"
  :group 'nix-mode
  :type 'function)

(defgroup nix-faces nil
  "Nix faces."
  :group 'nix-mode
  :group 'faces)

(defface nix-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight Nix keywords."
  :group 'nix-faces)

(defface nix-keyword-warning-face
  '((t :inherit font-lock-warning-face))
  "Face used to highlight Nix warning keywords."
  :group 'nix-faces)

(defface nix-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight Nix builtins."
  :group 'nix-faces)

(defface nix-constant-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight Nix constants."
  :group 'nix-faces)

(defface nix-attribute-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight Nix attributes."
  :group 'nix-faces)

(defface nix-antiquote-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight Nix antiquotes."
  :group 'nix-faces)

;;; Constants

(defconst nix-system-types
  '("x86_64-linux" "i686-linux" "aarch64-linux" "x86_64-darwin")
  "List of supported systems.")

(defconst nix-keywords
  '("if" "then"
    "else" "with"
    "let" "in"
    "rec" "inherit"
    "or"))

(defconst nix-builtins
  '("builtins" "baseNameOf"
    "derivation" "dirOf"
    "true" "false" "null"
    "isNull" "toString"
    "fetchTarball" "import"
    "map" "removeAttrs"))

(defconst nix-warning-keywords
  '("assert" "abort" "throw"))

;;; Regexps

(defconst nix-re-file-path
  "[a-zA-Z0-9._\\+-]*\\(/[a-zA-Z0-9._\\+-]+\\)+")

(defconst nix-re-url
  "[a-zA-Z][a-zA-Z0-9\\+-\\.]*:[a-zA-Z0-9%/\\?:@&=\\+\\$,_\\.!~\\*'-]+")

(defconst nix-re-bracket-path
  "<[a-zA-Z0-9._\\+-]+\\(/[a-zA-Z0-9._\\+-]+\\)*>")

(defconst nix-re-variable-assign
  "\\<\\([a-zA-Z_][a-zA-Z0-9_'\-\.]*\\)[ \t]*=[^=]")

(defconst nix-re-caps
  " =[ \n]\\|\(\\|\{\\|\\[\\|\\bwith\\b\\|\\blet\\b\\|\\binherit\\b")

(defconst nix-re-ends ";\\|\)\\|\\]\\|\}\\|\\bin\\b")

(defconst nix-re-quotes "''\\|\"")

(defconst nix-re-comments "#\\|/*\\|*/")

(defconst nix-font-lock-keywords
  `((,(regexp-opt nix-keywords 'symbols) 0 'nix-keyword-face)
    (,(regexp-opt nix-warning-keywords 'symbols) 0 'nix-keyword-warning-face)
    (,(regexp-opt nix-builtins 'symbols) 0 'nix-builtin-face)
    (,nix-re-url 0 'nix-constant-face)
    (,nix-re-file-path 0 'nix-constant-face)
    (,nix-re-variable-assign 1 'nix-attribute-face)
    (,nix-re-bracket-path 0 'nix-constant-face)
    (nix--syntax-match-antiquote 0 'nix-antiquote-face t))
  "Font lock keywords for nix.")

(defconst nix--variable-char "[a-zA-Z0-9_'\-]")

(defvar nix-mode-abbrev-table
  (make-abbrev-table)
  "Abbrev table for Nix mode.")

(defvar nix-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "'" table)
    (modify-syntax-entry ?- "'" table)
    (modify-syntax-entry ?' "'" table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    ;; We handle strings
    (modify-syntax-entry ?\" "." table)
    ;; We handle escapes
    (modify-syntax-entry ?\\ "." table)
    table)
  "Syntax table for Nix mode.")

(defun nix--syntax-match-antiquote (limit)
  "Find antiquote within a Nix expression up to LIMIT."
  (unless (> (point) limit)
    (if (get-text-property (point) 'nix-syntax-antiquote)
        (progn
          (set-match-data (list (point) (1+ (point))))
          (forward-char 1)
          t)
      (let ((pos (next-single-char-property-change (point) 'nix-syntax-antiquote
                                                   nil limit)))
        (when (and pos (not (> pos limit)))
          (goto-char pos)
          (let ((char (char-after pos)))
            (pcase char
              (`?{
               (forward-char 1)
               (set-match-data (list (1- pos) (point)))
               t)
              (`?}
               (forward-char 1)
               (set-match-data (list pos (point)))
               t))))))))

(defun nix--mark-string (pos string-type)
  "Mark string as a Nix string.

POS position of start of string
STRING-TYPE type of string based off of Emacs syntax table types"
  (put-text-property pos (1+ pos)
                     'syntax-table (string-to-syntax "|"))
  (put-text-property pos (1+ pos)
                     'nix-string-type string-type))

(defun nix--get-parse-state (pos)
  "Get the result of `syntax-ppss' at POS."
  (save-excursion (save-match-data (syntax-ppss pos))))

(defun nix--get-string-type (parse-state)
  "Get the type of string based on PARSE-STATE."
  (let ((string-start (nth 8 parse-state)))
    (and string-start (get-text-property string-start 'nix-string-type))))

(defun nix--open-brace-string-type (parse-state)
  "Determine if this is an open brace string type based on PARSE-STATE."
  (let ((open-brace (nth 1 parse-state)))
    (and open-brace (get-text-property open-brace 'nix-string-type))))

(defun nix--open-brace-antiquote-p (parse-state)
  "Determine if this is an open brace antiquote based on PARSE-STATE."
  (let ((open-brace (nth 1 parse-state)))
    (and open-brace (get-text-property open-brace 'nix-syntax-antiquote))))

(defun nix--single-quotes ()
  "Handle Nix single quotes."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (context (nix--get-parse-state start))
         (string-type (nix--get-string-type context)))
    (unless (or (equal string-type ?\")
                (and (equal string-type nil)
                     (< 1 start)
                     (string-match-p nix--variable-char
                                     (buffer-substring (1- start) start))))
      (when (equal string-type nil)
        (nix--mark-string start ?\')
        (setq start (+ 2 start)))
      (when (equal (mod (- end start) 3) 2)
        (let ((str-peek (buffer-substring end (min (point-max) (+ 2 end)))))
          (if (member str-peek '("${" "\\n" "\\r" "\\t"))
              (goto-char (+ 2 end))
            (nix--mark-string (1- end) ?\')))))))

(defun nix--escaped-antiquote-dq-style ()
  "Handle Nix escaped antiquote dq style."
  (let* ((start (match-beginning 0))
         (ps (nix--get-parse-state start))
         (string-type (nix--get-string-type ps)))
    (when (equal string-type ?\')
      (nix--antiquote-open-at (1+ start) ?\'))))

(defun nix--double-quotes ()
  "Handle Nix double quotes."
  (let* ((pos (match-beginning 0))
         (ps (nix--get-parse-state pos))
         (string-type (nix--get-string-type ps)))
    (unless (equal string-type ?\')
      (nix--mark-string pos ?\"))))

(defun nix--antiquote-open-at (pos string-type)
  "Handle Nix antiquote open at based on POS and STRING-TYPE."
  (put-text-property pos (1+ pos)
                     'syntax-table (string-to-syntax "|"))
  (put-text-property pos (+ 2 pos)
                     'nix-string-type string-type)
  (put-text-property (1+ pos) (+ 2 pos)
                     'nix-syntax-antiquote t))

(defun nix--antiquote-open ()
  "Handle Nix antiquote open."
  (let* ((start (match-beginning 0))
         (ps (nix--get-parse-state start))
         (string-type (nix--get-string-type ps)))
    (when string-type
      (nix--antiquote-open-at start string-type))))

(defun nix--antiquote-close-open ()
  "Handle Nix antiquote close then open."
  (let* ((start (match-beginning 0))
         (ps (nix--get-parse-state start))
         (string-type (nix--get-string-type ps)))
    (if string-type
        (nix--antiquote-open-at (1+ start) string-type)
      (when (nix--open-brace-antiquote-p ps)
        (let ((string-type (nix--open-brace-string-type ps)))
          (put-text-property start (+ 3 start)
                             'nix-string-type string-type)
          (put-text-property start (1+ start)
                             'nix-syntax-antiquote t)
          (put-text-property (+ 2 start) (+ 3 start)
                             'nix-syntax-antiquote t))))))

(defun nix--antiquote-close ()
  "Handle Nix antiquote close."
  (let* ((start (match-beginning 0))
         (ps (nix--get-parse-state start)))
    (unless (nix--get-string-type ps)
      (let ((string-type (nix--open-brace-string-type ps)))
        (when string-type
          (put-text-property start (1+ start)
                             'nix-string-type string-type)
          (put-text-property start (1+ start)
                             'nix-syntax-antiquote t)
          (let ((ahead (buffer-substring (1+ start)
                                         (min (point-max) (+ 5 start)))))
            (pcase string-type
              (`?\" (cond
                     ((or (string-match "^\\\\\"" ahead)
                          (string-match "^\\\\\\${" ahead))
                      (nix--mark-string (1+ start) string-type)
                      (goto-char (+ start (match-end 0) 1)))
                     ((string-match-p "^\"" ahead)
                      (goto-char (+ 2 start)))
                     ((< (1+ start) (point-max))
                      (nix--mark-string (1+ start) string-type)
                      (goto-char (+ 2 start)))))
              (`?\' (cond
                     ((or (string-match "^'''" ahead)
                          (string-match "^''\\${" ahead)
                          (string-match "^''\\\\[nrt]" ahead))
                      (nix--mark-string (1+ start) string-type)
                      (goto-char (+ start (match-end 0) 1)))
                     ((string-match-p "^''" ahead)
                      (goto-char (+ 3 start)))
                     ((< (1+ start) (point-max))
                      (nix--mark-string (1+ start) string-type)
                      (goto-char (+ 2 start))))))))))))

(defun nix-syntax-propertize (start end)
  "Special syntax properties for Nix from START to END."
  (goto-char start)
  (remove-text-properties start end
                          '(nix-string-type nil nix-syntax-antiquote nil))
  (funcall
   (syntax-propertize-rules
    ("\\\\\\\\"
     (0 nil))
    ("\\\\\""
     (0 nil))
    ("\\\\\\${"
     (0 (ignore (nix--escaped-antiquote-dq-style))))
    ("'\\{2,\\}"
     (0 (ignore (nix--single-quotes))))
    ("}\\${"
     (0 (ignore (nix--antiquote-close-open))))
    ("\\${"
     (0 (ignore (nix--antiquote-open))))
    ("}"
     (0 (ignore (nix--antiquote-close))))
    ("\""
     (0 (ignore (nix--double-quotes)))))
   start end))

;; Indentation using SMIE

(defconst nix-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (expr (arg ":" expr)
             ("if" expr "then" expr "else" expr)
             ("let" decls "in" expr)
             ("with" expr "nonsep-;" expr)
             ("assert" expr "nonsep-;" expr)
             (attrset)
             (id))
       (attrset ("{" decls "}"))
       (decls (decls ";" decls)
              (id "=" expr))
       (arg (id) ("{" args "}"))
       (args (args "," args) (id "arg-?" expr)))
     '((assoc ";"))
     '((assoc ","))
     ;; resolve "(with foo; a) <op> b" vs "with foo; (a <op> b)"
     ;; in favor of the latter.
     '((nonassoc "nonsep-;") (nonassoc " -dummy- "))
     ;; resolve "(if ... then ... else a) <op> b"
     ;; vs "if ... then ... else (a <op> b)" in favor of the latter.
     '((nonassoc "in") (nonassoc " -dummy- ")))
    (smie-precs->prec2
     '((nonassoc " -dummy- ")
       (nonassoc "=")
       ;; " -bexpskip- " and " -fexpskip- " are handy tokens for skipping over
       ;; whole expressions.
       ;; For instance, suppose we have a line looking like this:
       ;; "{ foo.bar // { x = y }"
       ;; and point is at the end of the line. We can skip the whole
       ;; expression (i.e. so the point is just before "foo") using
       ;; `(smie-backward-sexp " -bexpskip- ")'. `(backward-sexp)' would
       ;; skip over "{ x = y }", not over the whole expression.
       (right " -bexpskip- ")
       (left " -fexpskip- ")
       (nonassoc "else")
       (right ":")
       (right "->")
       (assoc "||")
       (assoc "&&")
       (nonassoc "==" "!=")
       (nonassoc "<" "<=" ">" ">=")
       (left "//")
       (nonassoc "!")
       (assoc "-" "+")
       (assoc "*" "/")
       (assoc "++")
       (left "?")
       ;; Tokens for skipping sequences of sexps
       ;; (i.e. identifiers or balanced parens).
       ;; For instance, suppose we have a line looking like this:
       ;; "{ foo.bar // f x "
       ;; and point is at the end of the line. We can skip the "f x"
       ;; part by doing `(smie-backward-sexp " -bseqskip- ")'.
       (right " -bseqskip- ")
       (left " -fseqskip- "))))))

(defconst nix-smie--symbols-re
  (regexp-opt '(":" "->" "||" "&&" "==" "!=" "<" "<=" ">" ">="
     "//" "-" "+" "*" "/" "++" "?" "=" "," ";" "!")))

(defconst nix-smie--infix-symbols-re
  (regexp-opt '(":" "->" "||" "&&" "==" "!=" "<" "<=" ">" ">="
                "//" "-" "+" "*" "/" "++" "?")))

(defconst nix-smie-indent-tokens-re
  (regexp-opt '("{" "(" "[" "=" "let" "if" "then" "else")))

;; The core indentation algorithm is very simple:
;; - If the last token on the previous line matches `nix-smie-indent-tokens-re',
;;   then the current line is indented by `tab-width' relative to the
;;   previous line's 'anchor'.
;; - Otherwise, let SMIE handle it.
;; The 'anchor' of a line is defined as follows:
;; - If the line contains an assignment, it is the beginning of the
;;   left-hand side of the first assignment on that line.
;; - Otherwise, it is the position of the first token on that line.
(defun nix-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:after . ,(guard (string-match-p nix-smie-indent-tokens-re
                                        token)))
     (nix-smie--indent-anchor))
    (`(:after . "in")
     (cond
      ((bolp) '(column . 0))
      ((<= (line-beginning-position)
           (save-excursion
             (forward-word)
             (smie-backward-sexp t)
             (point)))
       (smie-rule-parent))))
    (`(:after . "nonsep-;")
     (forward-char)
     (backward-sexp)
     (if (smie-rule-bolp)
         `(column . ,(current-column))
       (nix-smie--indent-anchor)))
    (`(:after . ":")
     ;; Skip over the argument.
     (smie-backward-sexp " -bseqskip- ")
     (if (smie-rule-bolp)
         `(column . ,(current-column))
       (nix-smie--indent-anchor)))
    (`(:after . ",")
     (smie-rule-parent tab-width))
    (`(:before . ",")
     ;; The parent is either the enclosing "{" or some previous ",".
     ;; In both cases this is what we want to align to.
     (smie-rule-parent))
    (`(:before . "if")
     (let ((bol (line-beginning-position)))
       (save-excursion
         (and
          (equal (nix-smie--backward-token) "else")
          (<= bol (point))
          `(column . ,(current-column))))))
    (`(:before . ,(guard (string-match-p nix-smie--infix-symbols-re token)))
     (forward-comment (- (point)))
     (let ((bol (line-beginning-position)))
       (smie-backward-sexp token)
       (if (< (point) bol)
           (nix-smie--indent-anchor 0))))))

(defun nix-smie--anchor ()
  "Return the anchor's offset from the beginning of the current line."
  (goto-char (+ (line-beginning-position) (current-indentation)))
  (let ((eol (line-end-position))
        (anchor (current-column))
        tok)
    (catch 'break
      (while (and (setq tok (car (smie-indent-forward-token)))
                  (<= (point) eol))
        (when (equal "=" tok)
          (backward-char)
          (smie-backward-sexp " -bseqskip- ")
          (setq anchor (current-column))
          (throw 'break nil))))
    anchor))

(defun nix-smie--indent-anchor (&optional indent)
  ;; Intended for use only in the rules function.
  (let ((indent (or indent tab-width)))
  `(column . ,(+ indent (nix-smie--anchor)))))

(defconst nix-smie--path-chars "a-zA-Z0-9-+_.:/~")

(defun nix-smie--skip-path (how)
  (let ((start (point)))
    (pcase how
      ('forward (skip-chars-forward nix-smie--path-chars))
      ('backward (skip-chars-backward nix-smie--path-chars))
      (_ (error "expected 'forward or 'backward")))
    (let ((sub (buffer-substring-no-properties start (point))))
      (if (string-match-p "/" sub)
          sub
        (ignore (goto-char start))))))

(defun nix-smie--forward-token-1 ()
  (forward-comment (point-max))
  (or (nix-smie--skip-path 'forward)
      (buffer-substring-no-properties
       (point)
       (progn
         (or (/= 0 (skip-syntax-forward "'w_"))
             (and (looking-at nix-smie--symbols-re)
                  (goto-char (match-end 0))))
         (point)))))

(defun nix-smie--forward-token ()
  (let ((sym (nix-smie--forward-token-1)))
    (cond
     ((equal sym ";")
      ;; The important lexer for indentation's performance is the backward
      ;; lexer, so for the forward lexer we delegate to the backward one.
      (save-excursion (nix-smie--backward-token)))
     (t sym))))

(defun nix-smie--backward-token-1 ()
  (forward-comment (- (point)))
  (or (nix-smie--skip-path 'backward)
      (buffer-substring-no-properties
       (point)
       (progn
         (or (/= 0 (skip-syntax-backward "'w_"))
             (and (looking-back nix-smie--symbols-re (- (point) 2) t)
                  (goto-char (match-beginning 0))))
         (point)))))

(defun nix-smie--backward-token ()
  (let ((sym (nix-smie--backward-token-1)))
    (unless (zerop (length sym))
      (pcase sym
        (";" (if (nix-smie--nonsep-semicolon-p) "nonsep-;" ";"))
        ("?" (if (nix-smie--arg-?-p) "arg-?" "?"))
        (_ sym)))))

(defun nix-smie--nonsep-semicolon-p ()
  "Whether the semicolon at point terminates a `with' or `assert'."
  (let (tok)
    (save-excursion
      ;; Skip over identifiers, balanced parens etc. as far back as we can.
      (while (null (setq tok (nth 2 (smie-backward-sexp " -bexpskip- "))))))
    (member tok '("with" "assert"))))

(defun nix-smie--arg-?-p ()
  "Whether the question mark at point is part of an argument declaration."
  (memq
   (nth 2 (progn
            (smie-backward-sexp)
            (smie-backward-sexp)))
   '("{" ",")))

(defun nix-smie--indent-close ()
  ;; Align close paren with opening paren.
  (save-excursion
    (when (looking-at "\\s)")
      (forward-char 1)
      (condition-case nil
          (progn
            (backward-sexp 1)
            ;; Align to the first token on the line containing
            ;; the opening paren.
            (current-indentation))
        (scan-error nil)))))

(defun nix-smie--indent-exps ()
  ;; This function replaces and is based on `smie-indent-exps'.
  ;; An argument to a function is indented relative to the function,
  ;; not to any other arguments.
  (save-excursion
    (let (parent   ;; token enclosing the expression list
          skipped) ;; whether we skipped at least one expression
      (let ((start (point)))
        (setq parent (nth 2 (smie-backward-sexp " -bseqskip- ")))
        (setq skipped (not (eq start (point))))
        (cond
         ((not skipped)
          ;; We're the first expression of the list.  In that case, the
          ;; indentation should be (have been) determined by its context.
          nil)
         ((equal parent "[")
          ;; It's a list, align with the first expression.
          (current-column))
         ;; We're an argument.
         (t
          ;; We can use (current-column) or (current-indentation) here.
          ;; (current-column) will indent relative to the first expression
          ;; in the sequence, and (current-indentation) will indent relative
          ;; to the indentation of the line on which the first expression
          ;; begins. I'm not sure which one is better.
          (+ tab-width (current-indentation))))))))

;;;###autoload
(defun nix-mode-format ()
  "Format the entire nix-mode buffer."
  (interactive)
  (when (eq major-mode 'nix-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (if (equal (string-match-p "^[\s-]*$" (thing-at-point 'line)) 0)
            (delete-horizontal-space)
          (smie-indent-line))
        (forward-line)))))

(defun nix-is-comment-p ()
  "Whether we are in a comment."
  (nth 4 (syntax-ppss)))

(defun nix-is-string-p ()
  "Whether we are in a string."
  (or (looking-at nix-re-quotes)
      (nix--get-string-type (nix--get-parse-state (point)))))

;;;###autoload
(defun nix-indent-region (start end)
  "Indent on a whole region. Enabled by default.
START where to start in region.
END where to end the region."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (when (and
                 ;; Skip if previous line is empty or a comment.
                 (save-excursion
                   (let ((line-is-comment-p (nix-is-comment-p)))
                     (forward-line -1)
                     (not
                      (or (and (nix-is-comment-p)
                               ;; Unless this line is a comment too.
                               (not line-is-comment-p))
                          (nix-is-comment-p)))))
                 ;; Don't mess with strings.
                 (nix-is-string-p))
            (smie-indent-line)))
      (forward-line 1))))

;;;###autoload
(defun nix-mode-ffap-nixpkgs-path (str)
  "Support `ffap' for <nixpkgs> declarations.
If STR contains brackets, call nix-instantiate to find the
location of STR. If nix-instantiate has a nonzero exit code,
don’t do anything"
  (when (and (string-match nix-re-bracket-path str)
             (executable-find nix-instantiate-executable))
    (with-temp-buffer
      (when (eq (call-process nix-instantiate-executable nil (current-buffer)
                              nil "--eval" "-E" str) 0)
        ;; Remove trailing newline
        (substring (buffer-string) 0 (- (buffer-size) 1))))))

;; Key maps

(defvar nix-mode-menu (make-sparse-keymap "Nix")
  "Menu for Nix mode.")

(defvar nix-mode-map (make-sparse-keymap)
  "Local keymap used for Nix mode.")

(defun nix-create-keymap ()
  "Create the keymap associated with the Nix mode."
  )

(defun nix-create-menu ()
  "Create the Nix menu as shown in the menu bar."
  (let ((m '("Nix"
             ["Format buffer" nix-format-buffer t])))
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
  :group 'nix-mode
  :syntax-table nix-mode-syntax-table
  :abbrev-table nix-mode-abbrev-table

  ;; Disable hard tabs and set tab to 2 spaces
  ;; Recommended by nixpkgs manual: https://nixos.org/nixpkgs/manual/#sec-syntax
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local electric-indent-chars '(?\n ?{ ?} ?\[ ?\] ?\( ?\)))

  ;; Font lock support.
  (setq-local font-lock-defaults '(nix-font-lock-keywords))

  ;; Special syntax properties for Nix
  (setq-local syntax-propertize-function 'nix-syntax-propertize)

  ;; Look at text properties when parsing
  (setq-local parse-sexp-lookup-properties t)

  ;; Automatic indentation [C-j]
  (smie-setup nix-smie-grammar 'nix-smie-rules
              :forward-token 'nix-smie--forward-token
              :backward-token 'nix-smie--backward-token)
  (setq-local smie-indent-basic 2)
  (setq-local indent-line-function 'smie-indent-line)
  (ignore-errors
    (setf (car (memq 'smie-indent-exps smie-indent-functions))
          'nix-smie--indent-exps)
    (setf (car (memq 'smie-indent-close smie-indent-functions))
          'nix-smie--indent-close))


  ;; Indenting of comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(^\\|\\s-\\);?#+ *")
  (setq-local comment-multi-line t)

  ;; Filling of comments
  (setq-local adaptive-fill-mode t)
  (setq-local paragraph-start "[ \t]*\\(#+[ \t]*\\)?$")
  (setq-local paragraph-separate paragraph-start)

  ;; Menu
  (easy-menu-add nix-mode-menu nix-mode-map)

  ;; Find file at point
  (push '(nix-mode . nix-mode-ffap-nixpkgs-path) ffap-alist)
  (push '(nix-mode "--:\\\\${}<>+@-Z_[:alpha:]~*?" "@" "@;.,!:")
        ffap-string-at-point-mode-alist))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(provide 'nix-mode)
;;; nix-mode.el ends here
