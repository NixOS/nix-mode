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
FILE, in `nix-mode'. All tests will use the `nix-indent-line'
function to do the indentation tests."

  `(with-temp-buffer
     ;; Read test data file
     (insert-file-contents (expand-file-name ,file nix-mode-test-dir))

     ;; Store the file as a local variable and set the right indentation function to use
     (let ((raw-file (buffer-substring-no-properties (point-min) (point-max)))
           (nix-indent-function 'nix-indent-line)
           (inhibit-message t))
       ;; Load up nix-mode
       (nix-mode)

       ;; If we're doing an indentation test
       (if ,indent
           (progn
             ;; Indent the buffer
             (indent-region (point-min) (point-max))

             ;; Compare buffer to the stored buffer contents
             (should (equal
                      (buffer-substring-no-properties (point-min) (point-max))
                      raw-file))))

       ;; Go to beginning
       (goto-char (point-min))

       ;; Run additional tests
       ,@body)))

(ert-deftest nix-mode-indent-test-list-contents ()
  "Proper indentation for items inside of a list."
  (with-nix-mode-test ("list-contents.nix" :indent t)))


(provide 'nix-mode-tests)
;;; nix-mode-tests.el ends here
