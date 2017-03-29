;;; nix-mode-mmm --- Summary

;;; Commentary:

;;; Code:

(require 'mmm-mode)

(mmm-add-group 'nix-sh
 '((sh-command
    :submode sh-mode
    :face mmm-output-submode-face
    :front "''"
    :back "''"
    :include-front t
    :front-offset 2
    )))

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'nix-mode "\\.nix\\'" 'nix-sh)

(provide 'nix-mode-mmm)
;;; nix-mode-mmm.el ends here
