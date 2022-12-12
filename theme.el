(require 'use-package)

;; (use-package pastelmac-theme
;;    :ensure t
;;    :config (load-theme 'pastelmac t))

;; (use-package color-theme-sanityinc-tomorrow
  ;; :ensure t
  ;; :config (load-theme 'sanityinc-tomorrow-blue))

;; (use-package solarized-theme
;;   :ensure t)

;; (use-package exotica-theme
;;   :ensure t
;;   :config
;;   (load-theme 'exotica t))

;; (use-package jazz-theme
;;   :ensure t)

(load-theme 'adwaita t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

(use-package all-the-icons
  :ensure t)

(setq-default cursor-type 'box)
(fringe-mode '(7 . 0))
(scroll-bar-mode -1)
(menu-bar-mode t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(display-battery-mode -1)
(display-time-mode +1)

(custom-set-faces
 '(company-tooltip ((t (:inherit default :background "#000000000000"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#0e451fb63780"))))
 '(company-tooltip-scrollbar-track ((t (:background "#13812b594bdd"))))
 '(company-tooltip-selection ((t (:inherit highlight;font-lock-function-name-face
				  ))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "systemTealColor"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "Brown"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "Blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "Orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "Purple"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "indian red"))))
 '(default ((t (:inherit nil :stipple nil :background "#EDEDED" :foreground "#2E3436" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Cascadia Mono")))))

(provide 'theme)
