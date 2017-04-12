(require 'mmm-auto)

(defun nix-sh-extra-phases (name)
  (list (concat name "Phase") (concat "pre" (capitalize name))
        (concat "post" (capitalize name))))

(let* ((nix-sh-all-phases
        (apply #'append
               (mapcar 'nix-sh-extra-phases
                       '("unpack" "patch" "configure" "build"
                         "check" "install" "fixup"
                         "dist"))))
       (nix-sh-start-regexp
        (concat (mmm-regexp-opt nix-sh-all-phases t) " = ''")))
  (mmm-add-group 'nix-sh
                 '((sh-command
                    :submode sh-mode
                    :face mmm-code-submode-face
                    :front "buildPhase = ''"
                    :back "''"
                    )))
  )

(mmm-add-mode-ext-class 'nix-mode "\\.nix\\'" 'nix-sh)

(provide 'nix-sh)
;;; nix-mode-mmm.el ends here
