(require 'org)
(org-babel-load-file
  (expand-file-name "settings.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(custom-enabled-themes '(afternoon))
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c76e27de8d7dae67d81c03c30051a05af1caf9324a300f339b60c38c4408f48b" "6940d9d23a7343bf858f4607c39967d0aa850a52985f86346e1917c0856cdd55" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "ae31831917d4bc2975f8f8e3a4dcbb9c4965fccc9d7da311a27ad5f993bc71a0" "3b7551ce9639f49d50b7c1cb693ee6f43428066c7cda9c8b146e87e08d2955a3" default))
 '(exwm-floating-border-color "#646464")
 '(fancy-splash-image "/home/mmagueta/.emacs.d/logo.png")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#70c900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-mark-symbol)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'modus-themes-pseudo-header)
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(dashboard projectile use-package org-bullets eshell-syntax-highlighting eglot-fsharp dmenu))
 '(pdf-view-midnight-colors '("#ffffff" . "#100f10"))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff8059")
     (40 . "#feacd0")
     (60 . "#f78fe7")
     (80 . "#f4923b")
     (100 . "#eecc00")
     (120 . "#cfdf30")
     (140 . "#f8dec0")
     (160 . "#bfebe0")
     (180 . "#44bc44")
     (200 . "#70c900")
     (220 . "#6ae4b9")
     (240 . "#4ae8fc")
     (260 . "#00d3d0")
     (280 . "#c6eaff")
     (300 . "#2fafff")
     (320 . "#79a8ff")
     (340 . "#00bcff")
     (360 . "#b6a0ff")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((emacs) (emacs) (emacs)))
 '(warning-suppress-types '((emacs) (emacs)))
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#f4923b" "#70c900" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
