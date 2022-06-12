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
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

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

(use-package rainbow-delimiters
  :ensure t
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

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
  :hook (nix-mode . (lambda () (lsp)))
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (defun nix-repl-with-variable ()
    (interactive)
    (let ((variables (read-string "Nix repl variable to load: ")))
      (defcustom nix-repl-executable-args `("repl" ,variables)
	"Arguments to provide to nix-repl."
	:type '(repeat string))
      (nix-repl))))

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

;; (use-package sly
;;   :ensure t
;;   :config
;;   (setq inferior-lisp-program "/nix/store/774bf5ksbhl7pbwxznk1wncw13ymak6k-sbcl-2.2.4/bin/sbcl"))

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
	   company-quickhelp-color-foreground (color-lighten-name (face-attribute 'default :foreground) 0)
	   company-quickhelp-color-background (color-darken-name (face-attribute 'default :background) 20)
	   pos-tip-foreground-color (face-attribute 'default :foreground) ; set pos-tip font color to the same as the theme
	   company-tooltip-align-annotations t ; align annotations to the right tooltip border
	   company-quickhelp-delay '1.0
	   company-quickhelp-use-propertized-text t)
     (let ((bg (face-attribute 'default :background)))
       (custom-set-faces
	`(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 10)))))
	`(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
	`(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
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

;; org stuff
(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode t)
  (setq org-agenda-custom-commands
        '(("z" "Super view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Important"
                                   :tag "Important"
                                   :priority "A"
                                   :order 2)
                            (:name "Next to do"
                                   :todo "NEXT"
                                   :order 5)
                            (:name "Personal"
                                   :tag "@personal"
                                   :order 10)
                            (:name "Work"
                                   :tag "@work"
                                   :order 15)
                            (:name "To read"
                                   :tag "Read"
                                   :order 30)
                            (:name "Waiting"
                                   :todo "WAITING"
                                   :order 40)
                            (:name "Due Today"
                                   :deadline today
                                   :order 2))))))))))

(use-package org-bullets
  :ensure t
  :hook
  ((org-mode . org-bullets-mode)))

(use-package plantuml-mode
  :ensure t
  :after org
  :config
  (setq org-plantuml-jar-path (expand-file-name "/nix/store/v0h307n0wjp8hmr4dzyv07j0j7g6cb3v-plantuml-1.2022.3/lib/plantuml.jar"))
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
  (setq dashboard-startup-banner "/home/mmagueta/.emacs.d/sources/emacs.svg")
  (setq dashboard-banner-logo-title "Welcome to MageMacs, a magic GNU Emacs customization")
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
		          (projects . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-footer-messages '("Quod oculus non vidit, nec auris audivit - I Corinthios II,IX")))

(use-package transpose-frame
   :ensure t)
