;;; nix-mode.el --- Major mode for editing .nix files -*- lexical-binding: t -*-

;; Maintainer: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Version: 1.4.4
;; Keywords: nix, languages, tools, unix
;; Package-Requires: ((emacs "25.1") magit-section (transient "0.3"))

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
  "Nix mode customizations."
  :group 'nix)

(defcustom nix-indent-function 'smie-indent-line
  "The function to use to indent.

Valid functions for this are:

- ‘indent-relative’
- ‘nix-indent-line' (buggy)
- `smie-indent-line' (‘nix-mode-use-smie’ must be enabled)"
  :group 'nix-mode
  :type 'function)

(defcustom nix-mode-use-smie t
  "Whether to use SMIE when editing Nix files.
This is enabled by default, but can take a while to load with
very large Nix files (all-packages.nix)."
  :group 'nix-mode
  :type 'boolean)

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
    "map" "removeAttrs"
    "toString" "derivationStrict" "placeholder" "scopedImport" "fromTOML"
    "fetchTarball" "fetchGit" "fetchTree" "fetchMercurial"))

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

(defun nix-re-keywords (keywords)
  "Produce a regexp matching some keywords of Nix.
KEYWORDS a list of strings to match as Nix keywords."
  (concat
   "\\(?:[[:space:];:{}()]\\|^\\)"
   (regexp-opt keywords t)
   "\\(?:[[:space:];:{}()]\\|$\\)"
   ))

(defconst nix-font-lock-keywords
  `((,(nix-re-keywords nix-keywords) 1 'nix-keyword-face)
    (,(nix-re-keywords nix-warning-keywords) 1 'nix-keyword-warning-face)
    (,(nix-re-keywords nix-builtins) 1 'nix-builtin-face)
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
    (modify-syntax-entry ?- "_" table)
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

(defconst nix-smie--2char-symbols
  '("->" "||" "&&" "==" "!=" "<=" ">=" "++" "//"))

(defconst nix-smie--infix-symbols-re
  (regexp-opt (append '(":" "<" ">" "-" "+" "*" "/" "?")
                      nix-smie--2char-symbols)))

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
  "Core smie rules."
  (pcase (cons kind token)
    (`(:after . ,(guard (string-match-p nix-smie-indent-tokens-re
                                        token)))
     (nix-smie--indent-anchor))
    (`(,_ . "in")
     (let ((bol (line-beginning-position)))
       (forward-word)
       ;; Go back to the corresponding "let".
       (smie-backward-sexp t)
       (pcase kind
         (:before
          (if (smie-rule-hanging-p)
              (nix-smie--indent-anchor 0)
            `(column . ,(current-column))))
         (:after
          (cond
           ((bolp) '(column . 0))
           ((<= bol (point))
            `(column . ,(current-column))))))))
    (`(:after . "nonsep-;")
     (forward-char)
     (backward-sexp)
     (if (smie-rule-bolp)
         `(column . ,(current-column))
       (nix-smie--indent-anchor)))
    (`(:after . ":")
     (or (nix-smie--indent-args-line)
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
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position))
          anchor
          tok)
      (forward-comment (point-max))
      (unless (or (eobp) (< eol (point)))
        (setq anchor (current-column))
        (catch 'break
          (while (and (not (eobp))
                      (progn
                        (setq tok (car (smie-indent-forward-token)))
                        (<= (point) eol)))
            (when (equal "=" tok)
              (backward-char)
              (smie-backward-sexp " -bseqskip- ")
              (setq anchor (current-column))
              (throw 'break nil))))
        anchor))))

(defun nix-smie--indent-anchor (&optional indent)
  "Intended for use only in the rules function."
  (let ((indent (or indent tab-width)))
    `(column . ,(+ indent (nix-smie--anchor)))))

(defun nix-smie--indent-args-line ()
  "Indent the body of a lambda whose argument(s) are on a line of their own."
  (save-excursion
    ;; Assume that point is right before ':', skip it
    (forward-char)
    (let ((tok ":"))
      (catch 'break
        (while (equal tok ":")
          (setq tok (nth 2 (smie-backward-sexp t)))
          (when (smie-rule-bolp)
            (throw 'break `(column . ,(current-column)))))))))

(defconst nix-smie--path-chars "a-zA-Z0-9-+_.:/~")

(defun nix-smie--skip-angle-path-forward ()
  "Skip forward a path enclosed in angle brackets, e.g <nixpkgs>."
  (let ((start (point)))
    (when (eq (char-after) ?<)
      (forward-char)
      (if (and (nix-smie--skip-path 'forward t)
               (eq (char-after) ?>))
          (progn
            (forward-char)
            (buffer-substring-no-properties start (point)))
        (ignore (goto-char start))))))

(defun nix-smie--skip-angle-path-backward ()
  "Skip backward a path enclosed in angle brackets, e.g <nixpkgs>."
  (let ((start (point)))
    (when (eq (char-before) ?>)
      (backward-char)
      (if (and (nix-smie--skip-path 'backward t)
               (eq (char-before) ?<))
          (progn
            (backward-char)
            (buffer-substring-no-properties start (point)))
        (ignore (goto-char start))))))

(defun nix-smie--skip-path (how &optional no-sep-check)
  "Skip path related characters."
  (let ((start (point)))
    (pcase-exhaustive how
      ('forward (skip-chars-forward nix-smie--path-chars))
      ('backward (skip-chars-backward nix-smie--path-chars)))
    (let ((sub (buffer-substring-no-properties start (point))))
      (if (or (and no-sep-check
                   (< 0 (length sub)))
              (string-match-p "/" sub))
          sub
        (ignore (goto-char start))))))

;; Returns non-nil if it successfully skipped a symbol.
(defun nix-smie--skip-symbol (how)
  (let* ((start (point))
         (nskip (pcase-exhaustive how
                  ('backward (skip-syntax-backward "._"))
                  ('forward  (skip-syntax-forward "._"))))
         (abs-skip (abs nskip)))
    (or (= 1 abs-skip)
        (and (= 2 abs-skip)
             (member (buffer-substring-no-properties (point) start)
                     nix-smie--2char-symbols))
        (if (< 0 abs-skip)
            (goto-char (+ start (if (< 0 nskip) 1 -1)))
          (goto-char start)
          nil))))

(defun nix-smie--forward-token-1 ()
  "Move forward one token."
  (forward-comment (point-max))
  (or (nix-smie--skip-angle-path-forward)
      (nix-smie--skip-path 'forward)
      (buffer-substring-no-properties
       (point)
       (progn
         (or (/= 0 (skip-syntax-forward "'w_"))
             (nix-smie--skip-symbol 'forward))
         (point)))))

(defun nix-smie--forward-token ()
  "Move forward one token, skipping certain characters."
  (let ((sym (nix-smie--forward-token-1)))
    (if (member sym '(";" "?"))
        ;; The important lexer for indentation's performance is the backward
        ;; lexer, so for the forward lexer we delegate to the backward one.
        (save-excursion (nix-smie--backward-token))
      sym)))

(defun nix-smie--backward-token-1 ()
  "Move backward one token."
  (forward-comment (- (point)))
  (or (nix-smie--skip-angle-path-backward)
      (nix-smie--skip-path 'backward)
      (buffer-substring-no-properties
       (point)
       (progn
         (or (/= 0 (skip-syntax-backward "'w_"))
             (nix-smie--skip-symbol 'backward))
         (point)))))

(defun nix-smie--backward-token ()
  "Move backward one token, skipping certain characters."
  (let ((sym (nix-smie--backward-token-1)))
    (unless (zerop (length sym))
      (pcase sym
        (";" (if (nix-smie--nonsep-semicolon-p) "nonsep-;" ";"))
        ("?" (if (nix-smie--arg-?-p) "arg-?" "?"))
        (_ sym)))))

(defun nix-smie--nonsep-semicolon-p ()
  "Whether the semicolon at point terminates a `with' or `assert'."
  (save-excursion
    (member (nth 2 (smie-backward-sexp " -bexpskip- ")) '("with" "assert"))))

(defun nix-smie--arg-?-p ()
  "Whether the question mark at point is part of an argument declaration."
  (member
   (nth 2 (progn
            (smie-backward-sexp)
            (smie-backward-sexp)))
   '("{" ",")))

(defun nix-smie--eol-p ()
  "Whether there are no tokens after point on the current line."
  (let ((eol (line-end-position)))
    (save-excursion
      (forward-comment (point-max))
      (or (eobp)
          (< eol (point))))))

(defun nix-smie--indent-close ()
  "Align close paren with opening paren."
  (save-excursion
    (when (looking-at "\\s)")
      (forward-char 1)
      (condition-case nil
          (progn
            (backward-sexp 1)
            ;; If the opening paren is not the last token on its line,
            ;; and it's either '[' or '{', align to the opening paren's
            ;; position. Otherwise, align its line's anchor.
            (if (and (memq (char-after) '(?\[ ?{))
                     (not (save-excursion (forward-char) (nix-smie--eol-p))))
                (current-column)
              (nix-smie--anchor)))
        (scan-error nil)))))

(defun nix-smie--indent-exps ()
  "This function replaces and is based on `smie-indent-exps'.
An argument to a function is indented relative to the function,
not to any other arguments."
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

;;; Indentation not using SMIE

(defun nix-find-backward-matching-token ()
  "Find the previous Nix token."
  (cond
   ((looking-at "in\\b")
    (let ((counter 1))
      (while (and (> counter 0)
                  (re-search-backward "\\b\\(let\\|in\\)\\b" nil t))
        (unless (or (nix--get-string-type (nix--get-parse-state (point)))
                    (nix-is-comment-p))
          (setq counter (cond ((looking-at "let") (- counter 1))
                              ((looking-at "in") (+ counter 1))))))
      counter ))
   ((looking-at "}")
    (backward-up-list) t)
   ((looking-at "]")
    (backward-up-list) t)
   ((looking-at ")")
    (backward-up-list) t)))

(defun nix-indent-to-backward-match ()
  "Match the previous line’s indentation."
  (let ((matching-indentation (save-excursion
                                (beginning-of-line)
                                (skip-chars-forward "[:space:]")
                                (if (nix-find-backward-matching-token)
                                    (current-indentation)))))
    (when matching-indentation (indent-line-to matching-indentation) t)))

(defun nix-indent-first-line-in-block ()
  "Indent the first line in a block."

  (let ((matching-indentation (save-excursion
                                ;; Go back to previous line that contain anything useful to check the
                                ;; contents of that line.
                                (beginning-of-line)
                                (skip-chars-backward "\n[:space:]")

                                ;; Grab the full string of the line before the one we're indenting
                                (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                                  ;; Then regex-match strings at the end of the line to detect if we need to indent the line after.
                                  ;; We could probably add more things to look for here in the future.
                                  (if (or (string-match "\\blet$" line)
                                          (string-match "\\bimport$" line)
                                          (string-match "\\[$" line)
                                          (string-match "=$" line)
                                          (string-match "\($" line)
                                          (string-match "\{$" line))

                                      ;; If it matches any of the regexes above, grab the indent level
                                      ;; of the line and add 2 to ident the line below this one.
                                      (+ 2 (current-indentation)))))))
    (when matching-indentation (indent-line-to matching-indentation) t)))

(defun nix-mode-search-backward ()
  "Search backward for items of interest regarding indentation."
  (re-search-backward nix-re-ends nil t)
  (re-search-backward nix-re-quotes nil t)
  (re-search-backward nix-re-caps nil t))

(defun nix-indent-expression-start ()
  "Indent the start of a nix expression."
  (let* ((ends 0)
         (once nil)
         (done nil)
         (indent (current-indentation)))
    (save-excursion
      ;; we want to indent this line, so we don't care what it
      ;; contains skip to the beginning so reverse searching doesn't
      ;; find any matches within
      (beginning-of-line)
      ;; search backward until an unbalanced cap is found or no cap or
      ;; end is found
      (while (and (not done) (nix-mode-search-backward))
        (cond
         ((looking-at nix-re-quotes)
          ;; skip over strings entirely
          (re-search-backward nix-re-quotes nil t))
         ((looking-at nix-re-comments)
          ;; skip over comments entirely
          (re-search-backward nix-re-comments nil t))
         ((looking-at nix-re-ends)
          ;; count the matched end
          ;; this means we expect to find at least one more cap
          (setq ends (+ ends 1)))
         ((looking-at nix-re-caps)
          ;; we found at least one cap
          ;; this means our function will return true
          ;; this signals to the caller we handled the indentation
          (setq once t)
          (if (> ends 0)
              ;; this cap corresponds to a previously matched end
              ;; reduce the number of unbalanced ends
              (setq ends (- ends 1))
            ;; no unbalanced ends correspond to this cap
            ;; this means we have found the expression that contains our line
            ;; we want to indent relative to this line
            (setq indent (current-indentation))
            ;; signal that the search loop should exit
            (setq done t))))))
    ;; done is t when we found an unbalanced expression cap
    (when done
      ;; indent relative to the indentation of the expression
      ;; containing our line
      (indent-line-to (+ tab-width indent)))
    ;; return t to the caller if we found at least one cap
    ;; this signals that we handled the indentation
    once))

(defun nix-indent-prev-level ()
  "Get the indent level of the previous line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward "\n[:space:]")
    (current-indentation)))

;;;###autoload
(defun nix-mode-format ()
  "Format the entire `nix-mode' buffer."
  (interactive)
  (when (eq major-mode 'nix-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (if (equal (string-match-p "^[\s-]*$" (thing-at-point 'line)) 0)
            (delete-horizontal-space)
          (nix-indent-line))
        (forward-line)))))

;;;###autoload
(defun nix-indent-line ()
  "Indent current line in a Nix expression."
  (interactive)
  (let ((end-of-indentation
         (save-excursion
           (cond
            ;; Indent first line of file to 0
            ((= (line-number-at-pos) 1)
             (indent-line-to 0))

            ;; comment
            ((save-excursion
               (beginning-of-line)
               (nix-is-comment-p))
             (indent-line-to (nix-indent-prev-level)))

            ;; string
            ((save-excursion
               (beginning-of-line)
               (nth 3 (syntax-ppss)))
             (indent-line-to (+ (nix-indent-prev-level)
                                (* tab-width
                                   (+ (if (save-excursion
                                            (forward-line -1)
                                            (end-of-line)
                                            (skip-chars-backward "[:space:]")
                                            (looking-back "''" 0)) 1 0)
                                      (if (save-excursion
                                            (beginning-of-line)
                                            (skip-chars-forward
                                             "[:space:]")
                                            (looking-at "''")
                                            ) -1 0)
                                      )))))

            ;; dedent '}', ']', ')' 'in'
            ((nix-indent-to-backward-match))

            ;; indent line after 'let', 'import', '[', '=', '(', '{'
            ((nix-indent-first-line-in-block))

            ;; indent between = and ; + 2, or to 2
            ((nix-indent-expression-start))

            ;; else
            (t
             (indent-line-to (nix-indent-prev-level))))
           (point))))
    (when (> end-of-indentation (point)) (goto-char end-of-indentation))))

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
  (interactive (list (region-beginning) (region-end)))
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
            (funcall nix-indent-function)))
      (forward-line 1))))

;;;###autoload
(defun nix-mode-ffap-nixpkgs-path (str)
  "Support `ffap' for <nixpkgs> declarations.
If STR contains brackets, call `nix-instantiate' to find the
location of STR. If `nix-instantiate' has a nonzero exit code,
don’t do anything"
  (when (and (string-match nix-re-bracket-path str)
             (executable-find nix-instantiate-executable))
    (let ((nix-executable nix-instantiate-executable))
      (ignore-errors
	(nix--process-string "--eval" "-E" str)))))

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
    (easy-menu-define nix-mode-menu nix-mode-map "Menu keymap for Nix mode" m)))

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

\\{nix-mode-map}"
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

  ;; Setup SMIE integration
  (when nix-mode-use-smie
    (smie-setup nix-smie-grammar 'nix-smie-rules
                :forward-token 'nix-smie--forward-token
                :backward-token 'nix-smie--backward-token)
    (setq-local smie-indent-basic 2)

    (let ((nix-smie-indent-functions
           ;; Replace the smie-indent-* equivalents with nix-mode's.
           (mapcar (lambda (fun) (pcase fun
                                   ('smie-indent-exps  'nix-smie--indent-exps)
                                   ('smie-indent-close 'nix-smie--indent-close)
                                   (_ fun)))
                   smie-indent-functions)))
      (setq-local smie-indent-functions nix-smie-indent-functions)))

  ;; Automatic indentation [C-j]
  (setq-local indent-line-function
              (lambda ()
                (if (and (not nix-mode-use-smie)
                         (eq nix-indent-function 'smie-indent-line))
                    (indent-relative)
                  (funcall nix-indent-function))))

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
  (push '(nix-mode "--:\\\\$<>+@-Z_[:alpha:]~*?" "@" "@;.,!:")
        ffap-string-at-point-mode-alist))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(provide 'nix-mode)
;;; nix-mode.el ends here
