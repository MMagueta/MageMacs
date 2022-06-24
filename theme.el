(require 'use-package)

(use-package pastelmac-theme
   :ensure t
   :config (load-theme 'pastelmac t))

;; (use-package color-theme-sanityinc-tomorrow
;;  :ensure t)

;; (use-package solarized-theme
;;   :ensure t)

;; (use-package exotica-theme
;;   :ensure t
;;   :config
;;   (load-theme 'exotica t))

;; (use-package jazz-theme
;;   :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package all-the-icons
  :ensure t)

(setq-default cursor-type 'box)
(setq fringe-mode 'left-only)
(scroll-bar-mode -1)
(menu-bar-mode +1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(display-battery-mode -1)
(display-time-mode t)

(custom-set-faces
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

(provide 'theme)
