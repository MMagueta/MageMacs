    (require 'use-package)

    (custom-set-variables
        '(package-selected-packages
            '(transpose-frame vscode-icon use-package tuareg swiper smartparens slime shrface scheme-complete rainbow-mode rainbow-delimiters rainbow-blocks racket-mode python-mode projectile powerline plantuml-mode org-drill org-bullets nix-mode multiple-cursors magit lsp-ui lsp-python-ms lsp-java lfe-mode inf-elixir hy-mode helm geiser fsharp-mode flycheck eshell-syntax-highlighting elixir-yasnippets elixir-mode elfeed eglot dired-sidebar dashboard company-quickhelp color-theme-sanityinc-tomorrow cider)))
  
    (use-package color-theme-sanityinc-tomorrow
       :ensure t)
    (load-theme 'sanityinc-tomorrow-blue t)
   
     (require 'lsp-mode)
     (defun lsp-wrapper ()
       (let* ((programming-modes '(fsharp-mode
				   tuareg-mode
				   nix-mode)))
	 (cond ((member major-mode programming-modes) (lsp))
	       (t nil))))
     (use-package lsp-mode
       :ensure t
       :config
       (add-hook 'lsp-mode-hook
		 (lambda ()
		   (add-hook 'before-save-hook
			     (lambda () (lsp-format-buffer buffer-file-name))
			     nil 'local))))
     (use-package prog-mode
       :config
       (add-hook 'prog-mode-hook #'lsp-wrapper))
   

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
       ;; This is based on
       ;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
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
       :config (company-quickhelp-mode nil))
     
     (use-package fsharp-mode
       :ensure t
       :mode (("\\.fs$" .  fsharp-mode)
	      ("\\.fsx$" .  fsharp-mode))
       :hook (
	      (fsharp-mode . company-mode))
       :config
       (setq inferior-fsharp-program "dotnet fsi")
       (add-hook 'prog-mode-hook
	       (lambda ()
		 (add-hook 'before-save-hook
			   (lambda () (lsp-format-buffer buffer-file-name))
			   nil 'local)))
       (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history))
   
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((python . t)))

   
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

   
     (use-package plantuml-mode
       :ensure t
       :init
       (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/sources/plantuml-jar-gplv2-1.2021.8/plantuml.jar"))
       (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
       (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

  
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
      (setq org-agenda-files (list "~/.emacs.d/Agenda/work.org"
				   "~/.emacs.d/Agenda/personal.org"))
      (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)"))))
    
    (use-package org-bullets
      :ensure t
      :hook
      ((org-mode-hook . (lambda () (org-bullets-mode 1)))))

  
    (unless (package-installed-p 'projectile)
      (package-install 'projectile))
    (require 'projectile)

    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

    (use-package dired-sidebar
      :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
      :ensure t
      :commands (dired-sidebar-toggle-sidebar)
      :init
      (add-hook 'dired-sidebar-mode-hook
		(lambda ()
		  (unless (file-remote-p default-directory)
		    (auto-revert-mode))))
      :config
      (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
      (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

      (setq dired-sidebar-theme 'vscode)
      (setq dired-sidebar-use-term-integration t)
      (setq dired-sidebar-use-custom-font t))

    (defun sidebar-toggle ()
      "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
      (interactive)
      (dired-sidebar-toggle-sidebar)
      (ibuffer-sidebar-toggle-sidebar))

   (use-package dashboard
      :ensure t
      :diminish dashboard-mode
      :config
      (setq dashboard-banner-logo-title "Welcome to MageMacs, a magic GNU Emacs customization")
      (setq dashboard-startup-banner "~/.emacs.d/sources/images/emacs.svg")
      (setq dashboard-items '((recents  . 10)
			      (bookmarks . 10)
			      (projects . 10)))
      (dashboard-setup-startup-hook))
      (fringe-mode 1)
      (scroll-bar-mode -1)
  
     (menu-bar-mode -1)
     (tool-bar-mode -1)
     (toggle-scroll-bar -1)
     (add-hook 'prog-mode-hook 'linum-mode)
     (display-battery-mode t)
     (display-time-mode t)
     (unless (package-installed-p 'vscode-icon)
     (package-install 'vscode-icon))
     (require 'vscode-icon)
     (use-package transpose-frame
         :ensure t)

(custom-set-faces
 '(default ((t (:family "Monaco" :foundry "APPL" :slant normal :weight normal :height 120 :width normal)))))
     ;;(custom-set-faces'(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight bold :height 120 :width normal)))))
     (unless (package-installed-p 'powerline)
       (package-install 'powerline))
     (require 'powerline)
     (powerline-default-theme)
     (display-battery-mode -1)
