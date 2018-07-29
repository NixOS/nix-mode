;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (eval nix-shell-with-string "let pkgs = import <nixpkgs> {};
in pkgs.runCommand \"nix-mode-shell\" {
  nativeBuildInputs = with pkgs; [ emacs git nix ];
} \"\"")))
