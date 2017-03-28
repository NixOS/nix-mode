;;; nix-flycheck.el --- Flycheck support for Nix.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'flycheck)

(defconst nix-err-msg-re
  "error: \\(.*\\) at \\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")

(defun nix--parse-errors (output checker buffer)
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (let ((errs '()))
      (while (search-forward-regexp nix-err-msg-re nil t 1)
        (let* ((file (match-string 2))
               (line (string-to-number (match-string 3)))
               (col (string-to-number (match-string 4)))
               (msg (match-string 1)))
          (setq errs
                (cons (flycheck-error-new-at
                       line col 'error msg
                       :filename (and (not (string= file "(string)")) file)
                       :checker checker
                       :buffer buffer)
                      errs))))
      errs)))

(flycheck-def-args-var flycheck-nix-args (nix))

(flycheck-define-checker nix
			 "A syntax and evaluation checker for Nix using nix-instantiate."
			 :command ("nix-instantiate" "--eval" "--strict" "--show-trace" (eval flycheck-nix-args) "-")
			 :standard-input t
			 :error-parser nix--parse-errors
			 :modes (nix-mode)
			 )

(add-to-list 'flycheck-checkers 'nix)

(provide 'nix-flycheck)
;;; nix-flycheck.el ends here
