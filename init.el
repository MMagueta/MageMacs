(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (use-package pastelmac-theme
;;   :ensure t
;;   :config
;;   (load-theme 'pastelmac t))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-blue t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

(setq-default cursor-type 'bar)

(setq create-lockfiles nil)

(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    ;; (interactive)
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (use-package yasnippet-classic-snippets
    :ensure t)
  (yas-reload-all))

(use-package elsa
  :ensure t
  :hook
  (emacs-lisp-mode . (lambda () (flycheck-elsa-setup)))
  :config
  (use-package flycheck-elsa
    :ensure t
    :hook
    ((emacs-lisp-mode . (lambda () (flycheck-mode)))
     (emacs-lisp-mode . (lambda () (flymake-mode))))))

(use-package flycheck
  :ensure t
  :config
  (use-package flymake-flycheck
    :ensure t))

(use-package diff-hl
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-lens-mode)
  :init
  (add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'fsharp-mode)
					     (lsp-format-buffer))))
  :config
  (use-package lsp-treemacs
    :ensure t))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  ;; Enabling only some features
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-mode 1)
  ;; The modes below are optional
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(defun magueta/lsp-ui-doc-toggle ()
  "For some reason it is required to do at least once a call to lsp-ui-doc-show in order for this to work."
  (interactive)
  (let ((frame (lsp-ui-doc--get-frame)))
    (cond ((frame-visible-p frame) (lsp-ui-doc-hide))
	  (t (lsp-ui-doc-show)))))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 7)
  :bind
  (("s-?" . 'magueta/lsp-ui-doc-toggle)))

(use-package lsp-metals
  :ensure t)

(use-package scala-mode
  :hook (scala-mode . (lambda () (lsp))))

(use-package clojure-mode
  :hook (clojure-mode . (lambda () (lsp))))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package nix-mode
  :ensure t
  :hook (nix-mode . (lambda () (lsp)))
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
		    :major-modes '(nix-mode)
		    :server-id 'nix)))

(use-package tuareg
  :ensure t
  :hook (tuareg-mode . (lambda () (lsp))))

(use-package python-mode
  :ensure t
  :after lsp-python-ms
  :hook (python-mode . (lambda () (lsp)))
  :config
  (use-package lsp-python-ms
    :ensure t))

(use-package swift-mode
  :ensure t
  :hook (swift-mode . (lambda () (lsp)))
  :after lsp-mode
  :config  
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))


(use-package racket-mode
  :ensure t
  :hook (racket-mode . (lambda () (lsp)))
  :after lsp-mode)

(use-package swiper
  :ensure t
  :init
  (global-set-key (kbd "\C-s") 'swiper))
   
(use-package slime
  :ensure t
  :hook
  (lisp-mode . (lambda () (auto-complete-mode)))
  (slime-mode . (lambda () (set-up-slime-ac)))
  (slime-repl-mode . (lambda () (set-up-slime-ac)))
  (lisp-mode . (lambda () (company-mode)))
  :config
  (setq inferior-lisp-program "sbcl")
  (use-package ac-slime
    :ensure t
    :after slime)
  (use-package auto-complete
    :ensure t
    :after slime))
  
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
   (setq company-tooltip-limit 10 ; bigger popup window
	 company-tooltip-minimum-width 15
	 company-tooltip-align-annotations t ; align annotations to the right tooltip border
	 company-quickhelp-delay '1.0)
   :config
   (company-quickhelp-mode nil)
   (add-hook 'prog-mode-hook 'linum-relative-mode)
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

(setq fringe-mode 'left-only)
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
   '(auto-package-update linum-relative paredit smartparens rainbow-delimiters rainbow-mode geiser-racket flymake-racket racket-mode ac-slime flycheck-clojure clojure-mode sbt-mode dockerfile-mode purescript-mode dap-mode flymake-flycheck flycheck yaml-mode yasnippet-snippets yasnippet-classic-snippets use-package typescript-mode tuareg transpose-frame swiper swift-mode slime python-mode projectile powerline org-bullets nix-mode multiple-cursors magit lsp-ui lsp-treemacs lsp-sourcekit lsp-python-ms helm fsharp-mode eshell-syntax-highlighting dashboard company-quickhelp color-theme-sanityinc-tomorrow all-the-icons)))
