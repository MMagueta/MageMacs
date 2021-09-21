(require 'org)
(unless (file-exists-p "settings.el")
  (org-babel-tangle-file
    (expand-file-name "settings.org" user-emacs-directory))
  (load (expand-file-name "settings.el" user-emacs-directory)))
(custom-set-variables
 '(package-selected-packages
   '(transpose-frame yasnippet vscode-icon use-package tuareg swiper slime scheme-complete racket-mode python-mode projectile powerline plantuml-mode org-drill org-bullets nix-mode multiple-cursors magit lsp-ui lsp-python-ms lsp-java helm geiser flycheck eshell-syntax-highlighting eglot-fsharp dired-sidebar dashboard company-quickhelp color-theme-sanityinc-tomorrow cider)))
(custom-set-faces'(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal)))))

