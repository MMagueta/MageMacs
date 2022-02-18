(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (use-package pastelmac-theme
;;   :ensure t
;;   :config
;;   (load-theme 'pastelmac t))

;; (use-package color-theme-sanityinc-tomorrow
  ;; :ensure t
  ;; :config
  ;; (load-theme 'sanityinc-tomorrow-blue t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

;; (setq-default cursor-type 'bar)

(setq create-lockfiles nil)

(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (add-hook 'shell-mode-hook 'colorize-compilation-buffer))

(use-package yasnippet
  :ensure t)

(use-package elsa
  :ensure t
  :hook
  (emacs-lisp-mode . (lambda () (flycheck-elsa-setup))))

(use-package flycheck-elsa
  :ensure t
  :hook
  ((emacs-lisp-mode . (lambda () (flycheck-mode)))
   (emacs-lisp-mode . (lambda () (flymake-mode)))))

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet-classic-snippets
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package flymake-flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-ui-sideline-diagnostic-max-lines 4)
  (add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'fsharp-mode)
    (lsp-format-buffer)))))

;; For some reason it is required to do at least once
;; a call to lsp-ui-doc-show in order for this to work
(defun toggle-between-doc-show-hide ()
  (interactive)
  (let ((frame (lsp-ui-doc--get-frame)))
    (cond ((frame-visible-p frame) (lsp-ui-doc-hide))
          (t (lsp-ui-doc-show)))))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable t)
  :bind
  (("s-?" . 'toggle-between-doc-show-hide)))

(use-package lsp-treemacs
  :ensure t)

(use-package nix-mode
  :ensure t
  :hook (nix-mode . (lambda () (lsp))))

(use-package tuareg
  :ensure t
  :hook (tuareg-mode . (lambda () (lsp))))

(use-package lsp-python-ms
  :ensure t)

(use-package python-mode
  :ensure t
  :after lsp-python-ms
  :hook (python-mode . (lambda () (lsp))))

(use-package swift-mode
  :ensure t
  :hook (swift-mode . (lambda () (lsp))))

(use-package lsp-sourcekit
  :after lsp-mode
  :ensure t
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swiper
  :ensure t
  :init
  (global-set-key (kbd "\C-s") 'swiper))
   
;;(use-package slime
;;   :ensure t)
;; (setq inferior-lisp-program "sbcl")
   
(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
		   :major-modes '(nix-mode)
		   :server-id 'nix))
  
(use-package comint
   :config
   (defun comint-write-history-on-exit (process event)
	 (comint-write-input-ring)
	 (let ((buf (process-buffer process)))
	   (when (buffer-live-p buf)
	 (with-current-buffer buf
	   (insert (format "\nProcess %s %s" process event))))))
 
   (defun turn-on-comint-history ()
	 (let ((process (get-buffer-process (current-buffer))))
	   (when process
	 (setq comint-input-ring-file-name
		   (format "~/.emacs.d/inferior-%s-history"
			   (process-name process)))
	 (comint-read-input-ring)
	 (set-process-sentinel process
				   #'comint-write-history-on-exit))))
 
   (defun mapc-buffers (fn)
	 (mapc (lambda (buffer)
		 (with-current-buffer buffer
		   (funcall fn)))
	   (buffer-list)))
 
   (defun comint-write-input-ring-all-buffers ()
	 (mapc-buffers 'comint-write-input-ring))
 
   (add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)
   (add-hook 'kill-buffer-hook 'comint-write-input-ring))
 
(use-package company-quickhelp
   :ensure t
   :init
   (setq company-quickhelp-delay '1.0)
   :config
   (company-quickhelp-mode nil)
   (add-hook 'prog-mode-hook 'linum-mode)
   :hook
   ((emacs-lisp-mode . (lambda () (company-mode)))))
 
(use-package fsharp-mode
   :ensure t
   :mode (("\\.fs$"  .  fsharp-mode)
	  ("\\.fsx$" .  fsharp-mode)
	  ("\\.fsi$" .  fsharp-mode))
   :hook ((fsharp-mode      . (lambda () (lsp))))
   :bind
   (("C-c C-,"     . 'fsharp-shift-region-left)
    ("C-c C-."     . 'fsharp-shift-region-right)
    ("C-o"         . 'fsharp-newline-and-indent)
    ("C-c C-i"     . 'run-fsharp)
    ("C-c C-a"     . 'fsharp-find-alternate-file)
    ("M-h"         . 'fsharp-mark-phrase))
   :config
   (setq compile-command "dotnet watch run")
   (setq inferior-fsharp-program "dotnet fsi")
   (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history))
   
(use-package magit
   :ensure t
   :init
   (global-set-key (kbd "C-x g") 'magit-status))
   
(use-package helm
   :ensure t
   :init
   (helm-mode 1)
   :config
   (global-set-key (kbd "M-x") 'helm-M-x)
   (global-set-key (kbd "C-x b") 'helm-buffers-list))
   
(use-package multiple-cursors
   :ensure t
   :config
   (global-set-key (kbd "C-c m c") 'mc/edit-lines))

;; (use-package plantuml-mode
;;    :ensure t
;;    :init
;;    (setq org-plantuml-jar-path (expand-file-name "./sources/plantuml-jar-gplv2-1.2021.8/plantuml.jar"))
;;    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;;    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))
  
(use-package eshell-syntax-highlighting
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode +1))
  
(use-package org
  :ensure t
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done 'time)
  (setq org-agenda-files (list "./Agenda/work.org"
			      "./Agenda/personal.org"))
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)"))))

(use-package org-bullets
  :ensure t
  :hook
  ((org-mode-hook . (lambda () (org-bullets-mode)))))
  
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package all-the-icons
  :ensure t)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Welcome to MageMacs, a magic GNU Emacs customization")
  (setq dashboard-startup-banner "./sources/images/emacs.svg")
  (setq dashboard-items '((recents  . 10)
			  (bookmarks . 10)
		          (projects . 10)))
  (dashboard-setup-startup-hook)
  (setq dashboard-footer-messages '()))

(fringe-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode +1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(display-battery-mode t)
(display-time-mode t)

(use-package transpose-frame
   :ensure t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "APPL" :slant normal :weight normal :height 140 :width normal)))))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  (display-battery-mode -1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d543a5f82ce200d50bdce81b2ecc4db51422439ba7c0e6845483dd89566e4cf9" default))
 '(package-selected-packages
   '(flymake-flycheck flycheck yaml-mode ac-sly yasnippet-snippets yasnippet-classic-snippets use-package typescript-mode tuareg transpose-frame swiper swift-mode sly-repl-ansi-color sly-quicklisp slime python-mode projectile powerline org-bullets nix-mode multiple-cursors magit lsp-ui lsp-treemacs lsp-sourcekit lsp-python-ms helm fsharp-mode eshell-syntax-highlighting dashboard company-quickhelp color-theme-sanityinc-tomorrow all-the-icons)))
