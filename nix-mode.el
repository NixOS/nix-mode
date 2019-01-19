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
(require 'ffap)
(eval-when-compile (require 'subr-x))

(defgroup nix-mode nil
  "Nix mode customizations"
  :group 'nix)

(defcustom nix-indent-function 'indent-relative
  "The function to use to indent.

Valid functions for this are:

- ‘indent-relative’
- nix-indent-line (buggy)"
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

;;; Indentation

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
                                  (if (or (string-match "let$" line)
                                          (string-match "import$" line)
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
  "Format the entire nix-mode buffer."
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
            (nix-indent-line)))
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
  (setq-local indent-line-function nix-indent-function)

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
