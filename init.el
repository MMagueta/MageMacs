(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)

(load-file "~/.emacs.d/theme.el")
(load-file "~/.emacs.d/behaviour.el")
(load-file "~/.emacs.d/configuration.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "nil" :slant normal :weight regular :height 145 :width normal))))
 '(fill-column-indicator ((t (:foreground "#CCCCCC" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "systemTealColor"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "Brown"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "Blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "Orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "Purple"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "indian red"))))
 '(speedbar-selected-face ((t (:foreground "#119911" :underline t)))))
