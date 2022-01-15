(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package pastelmac-theme
  :ensure t
  :config
  (load-theme 'pastelmac t))

;; (setq-default cursor-type 'bar)

(setq create-lockfiles nil)

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t)

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
   :ensure t)
(global-set-key (kbd "\C-s") 'swiper)
   
(use-package slime
   :ensure t)
(setq inferior-lisp-program "sbcl")
   
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
   (add-hook 'prog-mode-hook 'linum-mode))
 
(use-package fsharp-mode
   :ensure t
   :mode (("\\.fs$" .  fsharp-mode)
	  ("\\.fsx$" .  fsharp-mode))
   :hook ((fsharp-mode . company-mode)
	  (fsharp-mode . (lambda () (lsp))))
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
  ((org-mode-hook . (lambda () (org-bullets-mode 1)))))
  
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
 '(package-selected-packages
   '(powerline transpose-frame dashboard all-the-icons projectile org-bullets eshell-syntax-highlighting multiple-cursors helm magit fsharp-mode company-quickhelp slime swiper lsp-sourcekit swift-mode python-mode lsp-python-ms tuareg nix-mode lsp-treemacs lsp-ui lsp-mode pastelmac-theme use-package)))
