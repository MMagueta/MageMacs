(use-package request
  :ensure t)

(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
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
  :init
  (add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'fsharp-mode)
					     (lsp-format-buffer))))
  :hook (lsp-mode . lsp-lens-mode)
  :config
  (use-package lsp-treemacs
    :ensure t))

;; (use-package dap-mode
;;   :commands (dap-debug dap-breakpoints-add)
;;   :init
;;   ;; (load-file "~/.emacs.d/sources/dap-netcore.el")
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-auto-configure-mode)
;;   (require 'dap-netcore)
;;   :custom
;;   (dap-netcore-install-dir "~/.emacs.d/.cache/"))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 7)
  (setq lsp-ui-sideline-diagnostic-max-line-length 30)
  (defun magueta/lsp-ui-doc-toggle ()
    "For some reason it is required to do at least once a call to lsp-ui-doc-show in order for this to work. Probably the problem resides on the frame created requiring some preparation before actually being used, so `frame-live-p` doesn't return nil."
    (interactive)
    (let ((frame (lsp-ui-doc--get-frame)))
      (if (frame-live-p frame)
	  (cond ((frame-visible-p frame) (lsp-ui-doc-hide))
		(t (lsp-ui-doc-show)))
	(message "Hover with the mouse or call `lsp-ui-doc-show` over some obect first. For why, read doc string."))))
  :bind
  (("s-?" . 'magueta/lsp-ui-doc-toggle)))

(use-package lsp-metals
  :ensure t)

(use-package scala-mode
  :ensure t
  :hook (scala-mode . (lambda () (lsp))))

(use-package erlang
  :ensure t
  :config
  (use-package company-erlang
    :ensure t)
  :hook
  (erlang-mode . (lambda () (lsp)))
  (haskell-mode . yas-minor-mode))

(use-package lfe-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (hy-mode . rainbow-delimiters-mode)
  (lfe-mode . rainbow-delimiters-mode))

;; (add-hook 'c-or-c++-mode #'(lambda () (lsp)))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . (lambda () (lsp)))
  :config
  (setq org-babel-clojure-backend 'cider)
  :init
  (use-package cider
    :ensure t))

;; (use-package purescript-mode
;;   :ensure t
;;   :hook (purescript-mode . (lambda () (lsp))))

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
  :init
  (defun nix-repl-with-variable ()
    (interactive)
    (let ((variables (read-string "Nix repl variable to load: ")))
      (defcustom nix-repl-executable-args `("repl" ,variables)
	"Arguments to provide to nix-repl."
	:type '(repeat string))
      (nix-repl)))
  :hook
  (nix-mode . (lambda () (lsp)))
  (nix-repl-mode . company-mode)
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package haskell-mode
  :ensure t
  :config
  (use-package lsp-haskell
    :ensure t)
  (use-package haskell-snippets
    :ensure t)
  :hook
  (haskell-mode . (lambda () (lsp)))
  (haskell-mode . yas-minor-mode))

(use-package tuareg
  :ensure t
  :hook (tuareg-mode . (lambda () (lsp)))
  :config
  (setq tuareg-match-patterns-aligned t)
  (setq tuareg-indent-align-with-first-arg nil))

;; (use-package python-mode
;;   :ensure t
;;   :config
;;   (use-package lsp-python-ms
;;     :ensure t
;;     :hook (python-mode . (lambda ()
;;                            (require 'lsp-python-ms)
;;                            (lsp)))
;;     :init
;;     (setq lsp-python-ms-executable (executable-find "python-language-server"))))

(use-package hy-mode
  :ensure t
  :mode (("\\.hy$"  .  hy-mode)))

;; (use-package swift-mode
;;   :ensure t
;;   :hook (swift-mode . (lambda () (lsp)))
;;   :after lsp-mode
;;   :config  
;;   (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

;; (use-package racket-mode
;;   :ensure t
;;   :hook (racket-mode . (lambda () (lsp)))
;;   :after lsp-mode)

(use-package swiper
  :ensure t
  :init
  (global-set-key (kbd "\C-s") 'swiper))
   
;; (use-package slime
;;   :ensure t
;;   :hook
;;   (lisp-mode . (lambda () (auto-complete-mode)))
;;   (slime-mode . (lambda () (set-up-slime-ac)))
;;   (slime-repl-mode . (lambda () (set-up-slime-ac)))
;;   (lisp-mode . (lambda () (company-mode)))
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (use-package ac-slime
;;     :ensure t
;;     :after slime)
;;   (use-package auto-complete
;;     :ensure t
;;     :after slime))

(use-package sly
   :ensure t)

(use-package linum-relative
  :ensure t)

(use-package company-quickhelp
   :ensure t
   :config
   (defun load-company-face ()
     (require 'color)
     (setq company-tooltip-limit 10
	   company-tooltip-flip-when-above t
	   company-tooltip-maximum-width 70
	   company-tooltip-minimum-width 15
	   company-quickhelp-color-foreground (color-lighten-name (face-attribute 'default :foreground) 10)
	   company-quickhelp-color-background (color-lighten-name (face-attribute 'default :background) 10)
	   pos-tip-foreground-color (face-attribute 'default :foreground) ; set pos-tip font color to the same as the theme
	   company-tooltip-align-annotations t ; align annotations to the right tooltip border
	   company-quickhelp-delay '1.0
	   company-quickhelp-use-propertized-text t)
     (let ((bg (face-attribute 'default :background)))
       (custom-set-faces
	`(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 10)))))
	`(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
	`(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 10)))))
	`(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
	`(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))
   (add-hook 'company-mode-hook 'load-company-face)
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
   (setq inferior-fsharp-program "dotnet fsi"))
   
(use-package csharp-mode
   :ensure t
   :hook ((csharp-mode . (lambda () (lsp)))))

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
   (global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)
   (global-set-key (kbd "C-c m c") 'mc/edit-lines))

(use-package eshell-syntax-highlighting
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package org
  :ensure t
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  ;; (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done 'time)
  (setq org-confirm-babel-evaluate nil))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; make some org commands available from anywhere (not only org mode)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package org-super-agenda
  :ensure t
  :init
  (org-super-agenda-mode)
  :config
  (setq org-agenda-span 21
	org-agenda-window-setup "only-window"
	org-agenda-files '("~/.emacs.d/PersonalAgenda.org")
	org-agenda-custom-commands '(("c" . "My Custom Agendas")
				     ("cu" "Unscheduled TODO"
				      ((todo "" ((org-agenda-overriding-header "\nUnscheduled TODO")
                                                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))) nil nil))
	org-agenda-prefix-format '((agenda . " %i %?-12t% s")
				   (todo . " %i %-12:c")
				   (tags . " %i %-12:c")
				   (search . " %i %-12:c")))
  (setq org-super-agenda-groups '((:name "Important" :priority "A")
				  (:name "Late" :scheduled past :order 1)
				  (:name "Planned for today" :scheduled today :order 1))))


(use-package org-bullets
  :ensure t
  :hook
  ((org-mode . org-bullets-mode)))

(use-package plantuml-mode
  :ensure t
  :after org
  :config
  (setq org-plantuml-jar-path (expand-file-name "/nix/store/*-plantuml-*/lib/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package dashboard
  :ensure t
  :after all-the-icons
  ;; :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  ;; (setq dashboard-set-file-icons t)
  ;; (setq dashboard-startup-banner "/Users/mmagueta/.emacs.d/sources/emacs.svg")
  (setq dashboard-banner-logo-title "Welcome to MageMacs, a magic GNU Emacs customization")
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
		          (projects . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-footer-messages '("Quod oculus non vidit, nec auris audivit - I Corinthios II,IX")))

(use-package transpose-frame
  :ensure t)

(use-package sml-mode
  :ensure t
  :config
  (use-package sml-basis
    :ensure t)
  (use-package sml-modeline
    :ensure t)
  (use-package ob-sml
    :ensure t))

(use-package proof-general
  :ensure t
  :config
  (use-package company-coq
    :ensure t)
  (use-package coq-commenter
    :ensure t))

(provide 'configuration)
;;; configuration.el ends here
