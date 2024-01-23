(require 'use-package)

;; ;; (require 'catppuccin-theme)
;; (use-package catppuccin-theme
;;   :straight t
;;   :custom catppuccin-flavor 'mocha
;;   :config (load-theme 'catppuccin t))
;; ;; (catppuccin-reload)

;; (use-package pastelmac-theme
;;    :straight t
;;    :config (load-theme 'pastelmac t))

;; (use-package color-theme-sanityinc-tomorrow
  ;; :straight t
  ;; :config (load-theme 'sanityinc-tomorrow-blue))

;; (use-package solarized-theme
;;   :straight t)

;; (use-package exotica-theme
;;   :straight t
;;   :config
;;   (load-theme 'exotica t))

;; (use-package jazz-theme
;;   :straight t)

(load-theme 'adwaita t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode t))

(use-package all-the-icons
  :straight t)

(setq-default cursor-type 'box)
(fringe-mode '(7 . 0))
(scroll-bar-mode -1)
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(display-battery-mode -1)
(display-time-mode +1)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "systemTealColor"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "Brown"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "Blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "Orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "Purple"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "indian red"))))
 (unless (eq system-type 'darwin) '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Cascadia Mono")))))
)

(provide 'theme)
