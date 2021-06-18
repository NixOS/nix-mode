;;; nix-mode-tests.el -- test nix-mode

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'nix-mode)

(ert-deftest nix-mode-quote-detection ()
  (should (with-temp-buffer
            (nix-mode)
            (insert "'' ")
            (save-excursion (insert " ''"))
            (eq (nix--get-string-type (nix--get-parse-state (point))) ?\'))))

(ert-deftest nix-mode-quote2-detection ()
  (should (with-temp-buffer
            (nix-mode)
            (insert "\"")
            (save-excursion (insert "\""))
            (eq (nix--get-string-type (nix--get-parse-state (point))) ?\"))))

(ert-deftest nix-mode-quote3-detection ()
  (should (with-temp-buffer
            (nix-mode)
            (eq (nix--get-string-type (nix--get-parse-state (point))) nil))))

(ert-deftest nix-smie-angle-path-backward-detection ()
  (should (with-temp-buffer
            (nix-mode)
            (insert "<nixpkgs/nixos>")
            (nix-smie--skip-angle-path-backward)
            (bobp))))

(ert-deftest nix-smie-angle-path-backward-invalid ()
  (should (with-temp-buffer
            (nix-mode)
            (insert "<nixpkgs/nixos>foo/bar>")
            (null (nix-smie--skip-angle-path-backward)))))

(ert-deftest nix-smie-angle-path-backward-early ()
  (should (with-temp-buffer
            (nix-mode)
            (insert "<nixpkgs/nixos<foo/bar>")
            (equal "<foo/bar>" (nix-smie--skip-angle-path-backward)))))

(ert-deftest nix-smie-angle-path-forward-detection ()
  (should (with-temp-buffer
            (nix-mode)
            (save-excursion (insert "<nixpkgs/nixos>"))
            (nix-smie--forward-token)
            (eobp))))

(ert-deftest nix-smie-angle-path-forward-invalid ()
  (should (with-temp-buffer
            (nix-mode)
            (save-excursion (insert "<nixpkgs/nixos<foo/bar>"))
            (null (nix-smie--skip-angle-path-forward)))))

(ert-deftest nix-smie-angle-path-forward-early ()
  (should (with-temp-buffer
            (nix-mode)
            (save-excursion (insert "<foo/bar>nixpkgs/nixos>"))
            (equal "<foo/bar>" (nix-smie--skip-angle-path-forward)))))

;;; Indentation tests

(defvar nix-mode-test-dir (expand-file-name "testcases"
                                            (if load-file-name
                                                (file-name-directory load-file-name)
                                              default-directory))
  "Directory containing the `nix-mode' testcase files.")

;; Define macro to build indentation tests
(cl-defmacro with-nix-mode-test ((file &key indent) &rest body)
  "Set up environment for testing `nix-mode'.
Execute BODY in a temporary buffer containing the contents of
FILE, in `nix-mode'. INDENT should be the desired value of
`nix-indent-function'. If it's non-nil, an indentation test
will be run before executing BODY."

  `(with-temp-buffer
     ;; Read test data file
     (insert-file-contents (expand-file-name ,file nix-mode-test-dir))

     ;; Store the file as a local variable and set the right indentation function to use
     (let ((raw-file (buffer-substring-no-properties (point-min) (point-max)))
           (nix-indent-function
            ,(or indent `',(default-value 'nix-indent-function)))
           (inhibit-message t))
       ;; Load up nix-mode
       (nix-mode)

       ;; If we're doing an indentation test
       (when ,indent
         ;; Indent the buffer
         (indent-region (point-min) (point-max))

         ;; Compare buffer to the stored buffer contents
         (should (equal
                  (buffer-substring-no-properties (point-min) (point-max))
                  raw-file)))

       ;; Go to beginning
       (goto-char (point-min))

       ;; Run additional tests
       ,@body)))

(ert-deftest nix-mode-test-indent-failed-ident ()
  "Proper indentation for items inside of a list."
  (with-nix-mode-test
   ;; File to read
   ("failed-ident-test.nix")

   ;; Indent the buffer
   (indent-region (point-min) (point-max))

   ;; Compare buffer to the stored buffer contents
   (should-not (equal
                (buffer-substring-no-properties (point-min) (point-max))
                raw-file))))

(ert-deftest nix-mode-test-indent-list-contents-smie ()
  "Proper indentation for items inside of a list."
  (with-nix-mode-test ("list-contents.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-list-contents ()
  "Proper indentation for items inside of a list."
  (with-nix-mode-test ("list-contents.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-issue-60-1-smie ()
  "Proper indentation of attrsets inside of lists inside of attrsets.

Related issue: https://github.com/NixOS/nix-mode/issues/60"
  (with-nix-mode-test ("issue-60.1.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-60-1 ()
  "Proper indentation of attrsets inside of lists inside of attrsets.

Related issue: https://github.com/NixOS/nix-mode/issues/60"
  (with-nix-mode-test ("issue-60.1.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-issue-60-2-smie ()
  "Proper indentation of code inside of let blocks.

Related issue: https://github.com/NixOS/nix-mode/issues/60"
  (with-nix-mode-test ("issue-60.2.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-60-2 ()
  "Proper indentation of code inside of let blocks.

Related issue: https://github.com/NixOS/nix-mode/issues/60"
  (with-nix-mode-test ("issue-60.2.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-issue-60-3-smie ()
  "Proper indentation of import and newline after equal.

Related issue: https://github.com/NixOS/nix-mode/issues/60"
  (with-nix-mode-test ("issue-60.3.nix" :indent 'smie-indent-line)))

;; nix-indent-line and smie-indent-line conflict, so we just use smie
;; (ert-deftest nix-mode-test-indent-issue-60-3 ()
;;   "Proper indentation of import and newline after equal.
;; Related issue: https://github.com/NixOS/nix-mode/issues/60"
;;   (with-nix-mode-test ("issue-60.3.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-issue-69-1-smie ()
  "Proper indentation of an empty attrset.

Related issue: https://github.com/NixOS/nix-mode/issues/69"
  (with-nix-mode-test ("issue-69.1.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-69-1 ()
  "Proper indentation of an empty attrset.

Related issue: https://github.com/NixOS/nix-mode/issues/69"
  (with-nix-mode-test ("issue-69.1.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-issue-69-2-smie ()
  "Proper indentation of an empty list.

Related issue: https://github.com/NixOS/nix-mode/issues/69"
  (with-nix-mode-test ("issue-60.2.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-69-2 ()
  "Proper indentation of an empty list.

Related issue: https://github.com/NixOS/nix-mode/issues/69"
  (with-nix-mode-test ("issue-60.2.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-issue-72-smie ()
  "Proper indentation of strings in a multi-line string.

Related issue: https://github.com/NixOS/nix-mode/issues/72"
  (with-nix-mode-test ("issue-72.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-72 ()
  "Proper indentation of strings in a multi-line string.

Related issue: https://github.com/NixOS/nix-mode/issues/72"
  (with-nix-mode-test ("issue-72.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-hello-smie ()
  "Proper indentation of a simple derivation."
  (with-nix-mode-test ("hello.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-hello ()
  "Proper indentation of a simple derivation."
  (with-nix-mode-test ("hello.nix" :indent 'nix-indent-line)))

(ert-deftest nix-mode-test-indent-all-packages-smie ()
  "Proper indentation of a set of package definitions."
  (with-nix-mode-test ("all-packages.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-74-smie ()
  "Proper indentation of a set of package definitions."
  (with-nix-mode-test ("issue-74.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-77-smie ()
  "Proper indentation of strings in a multi-line string."
  (with-nix-mode-test ("issue-77.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-78-smie ()
  "Proper indentation of strings in a multi-line string."
  (with-nix-mode-test ("issue-78.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-94 ()
  "Proper indentation of attrsets inside of lists inside of attrsets.

Related issue: https://github.com/NixOS/nix-mode/issues/94"
  (with-nix-mode-test ("issue-94.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-lambdas-smie ()
  "Proper indentation of function bodies."
  (with-nix-mode-test ("smie-lambdas.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-close-parens-smie ()
  "Proper indentation of closing parentheses."
  (with-nix-mode-test ("smie-close-parens.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-let-smie ()
  "Proper indentation of let expressions."
  (with-nix-mode-test ("smie-let.nix" :indent 'smie-indent-line)))

(ert-deftest nix-mode-test-indent-issue-128 ()
  "Proper indentation of let expressions."
  (with-nix-mode-test ("issue-128.nix" :indent 'nix-indent-line)))

(provide 'nix-mode-tests)
;;; nix-mode-tests.el ends here
