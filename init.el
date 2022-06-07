(load-file "~/.emacs.d/theme.el")
(load-file "~/.emacs.d/behaviour.el")
(load-file "~/.emacs.d/private.el")
(load-file "~/.emacs.d/configuration.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dap-mode jazz-theme solarized-theme color-theme-sanityinc-tomorrow pastelmac-theme yasnippet-snippets yasnippet-classic-snippets use-package tuareg transpose-frame swiper request rainbow-delimiters projectile powerline plantuml-mode org-super-agenda org-bullets nix-mode multiple-cursors magit lsp-ui lsp-treemacs linum-relative helm fsharp-mode flymake-flycheck flycheck-elsa exotica-theme eshell-syntax-highlighting elsa elfeed diff-hl dashboard csharp-mode company-quickhelp cider all-the-icons ac-slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#13812b594bdd"))) t)
 '(company-scrollbar-fg ((t (:background "#0e451fb63780"))) t)
 '(company-tooltip ((t (:inherit default :background "#000000000000"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#0e451fb63780"))))
 '(company-tooltip-scrollbar-track ((t (:background "#13812b594bdd"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "systemTealColor"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "Brown"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "Blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "Orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "Purple"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "indian red")))))
