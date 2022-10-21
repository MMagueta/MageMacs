
(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package yasnippet

  :config
  (use-package yasnippet-snippets
    )
  (use-package yasnippet-classic-snippets
    )
  (yas-reload-all))

(use-package elsa
  :hook
  (emacs-lisp-mode . (lambda () (flycheck-elsa-setup)))
  :config
  (use-package flycheck-elsa
    :hook
    ((emacs-lisp-mode . (lambda () (flycheck-mode)))
     (emacs-lisp-mode . (lambda () (flymake-mode))))))

(use-package flycheck

  :config
  (use-package flymake-flycheck
    ))

(use-package diff-hl
  )

(use-package lsp-mode
  :init
  (add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'fsharp-mode)
                                             (lsp-format-buffer))))
  :hook ((lsp-mode . lsp-lens-mode))
  :config
  (use-package lsp-treemacs))

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
  )

(use-package scala-mode
  :hook ((scala-mode . lsp-deferred)))

(use-package erlang
  :config
  (use-package company-erlang)
  :hook
  ((erlang-mode . lsp-deferred)
   (haskell-mode . yas-minor-mode)))

(use-package lfe-mode
  )

(use-package rainbow-delimiters

  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (hy-mode . rainbow-delimiters-mode)
  (lfe-mode . rainbow-delimiters-mode))

;; (add-hook 'c-or-c++-mode #'(lambda () (lsp)))

(use-package clojure-mode

  :hook (clojure-mode . lsp-deferred)
  :config
  (setq org-babel-clojure-backend 'cider)
  :init
  (use-package cider
    ))

;; (use-package purescript-mode
;;
;;   :hook (purescript-mode . (lambda () (lsp))))

(use-package nix-mode
  :init
  (defun nix-repl-with-variable ()
    (interactive)
    (let ((variables (read-string "Nix repl variable to load: ")))
      (defcustom nix-repl-executable-args `("repl" ,variables)
        "Arguments to provide to nix-repl."
        :type '(repeat string))
      (nix-repl)))
  :hook
  (nix-mode . lsp-deferred)
  (nix-repl-mode . company-mode)
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package haskell-mode

  :config
  (use-package lsp-haskell
    )
  (use-package haskell-snippets
    )
  :hook
  (haskell-mode . lsp-deferred)
  (haskell-mode . yas-minor-mode))

(use-package tuareg

  :hook (tuareg-mode . lsp-deferred)
  :config
  (setq tuareg-match-patterns-aligned t)
  (setq tuareg-indent-align-with-first-arg nil))

;; (use-package python-mode
;;
;;   :config
;;   (use-package lsp-python-ms
;;
;;     :hook (python-mode . (lambda ()
;;                            (require 'lsp-python-ms)
;;                            (lsp)))
;;     :init
;;     (setq lsp-python-ms-executable (executable-find "python-language-server"))))

(use-package hy-mode

  :mode (("\\.hy$"  .  hy-mode)))

;; (use-package swift-mode
;;
;;   :hook (swift-mode . (lambda () (lsp)))
;;   :after lsp-mode
;;   :config
;;   (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

;; (use-package racket-mode
;;
;;   :hook (racket-mode . (lambda () (lsp)))
;;   :after lsp-mode)

(use-package swiper

  :init
  (global-set-key (kbd "\C-s") 'swiper))

;; (use-package slime
;;
;;   :hook
;;   (lisp-mode . (lambda () (auto-complete-mode)))
;;   (slime-mode . (lambda () (set-up-slime-ac)))
;;   (slime-repl-mode . (lambda () (set-up-slime-ac)))
;;   (lisp-mode . (lambda () (company-mode)))
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (use-package ac-slime
;;
;;     :after slime)
;;   (use-package auto-complete
;;
;;     :after slime))

(use-package sly
  )

(use-package linum-relative
  )

(use-package company-quickhelp

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

  :mode (("\\.fs$"  .  fsharp-mode)
	 ("\\.fsx$" .  fsharp-mode)
	 ("\\.fsi$" .  fsharp-mode))
  :hook ((fsharp-mode      . lsp-deferred))
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

  :hook ((csharp-mode . lsp-deferred)))

(use-package magit

  :init
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package helm

  :init
  (helm-mode 1)
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list))

(use-package multiple-cursors

  :config
  (global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-c m c") 'mc/edit-lines))

(use-package eshell-syntax-highlighting

  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package org

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

  :hook
  ((org-mode . org-bullets-mode)))

(use-package plantuml-mode

  :after org
  :config
  (setq org-plantuml-jar-path (expand-file-name "/nix/store/*-plantuml-*/lib/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(use-package projectile

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package dashboard

  :after all-the-icons
  ;; :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  ;; (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "sources/emacs.svg")
  (setq dashboard-banner-logo-title "Welcome to MageMacs, a magic GNU Emacs customization")
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
		          (projects . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-footer-messages '("Quod oculus non vidit, nec auris audivit - I Corinthios II,IX")))

(use-package transpose-frame
  )

(use-package sml-mode

  :config
  (use-package sml-basis
    )
  (use-package sml-modeline
    )
  (use-package ob-sml
    ))

(provide 'configuration)
;;; configuration.el ends here
