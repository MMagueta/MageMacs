(use-package yasnippet
  :straight t)

(use-package flycheck
  :straight t
  ;; :custom (flycheck-check-syntax-automatically '(save mode-enable))
  )

(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-lens-mode)
	 ;; Solves F# buffer out of sync
	 ;; (lsp-mode . (lambda () (lsp-treemacs-sync-mode -1))
	 (lsp-mode . (flymake-mode-off))))

(use-package dap-mode
  :straight t
  :after lsp-mode)

(require 'dap-netcore)

(use-package lsp-ui
  :straight t
  :init
  (setq lsp-ui-doc-enable t
	lsp-ui-sideline-diagnostic-max-lines 7
	lsp-ui-sideline-diagnostic-max-line-length 40
	lsp-lens-enable nil)
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

(use-package nix-mode
  :straight t
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
  ;; (nix-repl-mode . company-mode)
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nil"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package swiper
  :straight t
  :bind
  ("\C-s" . swiper))

(use-package display-line-numbers
  :straight t
  :hook ((prog-mode . (lambda () (progn
				   (display-line-numbers-mode)
				   (setq display-line-numbers 'relative))))))

(use-package windmove
  :straight t
  :bind
  ("C-c <right>" . windmove-right)
  ("C-c <left>" . windmove-left)
  ("C-c <up>" . windmove-up)
  ("C-c <down>" . windmove-down))

(use-package perspective
  :straight t
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (persp-mode)
  :custom (persp-suppress-no-prefix-key-warning t))

(use-package move-text
  :straight t
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))

(use-package corfu
  :straight t
  :custom ((corfu-auto t)
	   (corfu-auto-defer 0.25)
	   (corfu-min-width 15)
	   (corfu-max-width 70)
	   (corfu-auto-prefix 1)
	   (corfu-popupinfo-defer corfu-auto-defer))
  :hook ((prog-mode . corfu-mode)
	 (corfu-mode . corfu-popupinfo-mode)
	 (eshell-mode . corfu-mode)))

(use-package fsharp-mode
   :straight t
   :mode (("\\.fs$"  .  fsharp-mode)
	  ("\\.fsx$" .  fsharp-mode)
	  ("\\.fsi$" .  fsharp-mode))
   :hook ((fsharp-mode . lsp-deferred))
   :bind
   (("C-c C-,"     . 'fsharp-shift-region-left)
    ("C-c C-."     . 'fsharp-shift-region-right)
    ("C-o"         . 'fsharp-newline-and-indent)
    ("C-c C-i"     . 'run-fsharp)
    ("C-c C-a"     . 'fsharp-find-alternate-file)
    ("M-h"         . 'fsharp-mark-phrase))
   :config
   (setq compile-command "dotnet build")
   (setq inferior-fsharp-program "dotnet fsi --readline-"))

(use-package magit
  :straight t
  :bind
  ("C-x g" . magit-status)
  :config
  (use-package diff-hl
    :straight t))

(use-package helm
  :straight t
  :init
  (helm-mode t)
  (set-face-attribute 'helm-selection nil
		      :background (color-lighten-name (face-attribute 'default :foreground) 50)
		      :foreground (color-darken-name (face-attribute 'default :background) 100))
  :bind
  ("M-x" . helm-M-x)
  ("M-i" . helm-imenu)
  ("C-x b" . helm-buffers-list))

(use-package multiple-cursors
  :straight t
  :bind
  ("C-c m c" . mc/edit-lines)
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this))

(use-package projectile
  :straight t
  :init
  (projectile-mode t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package dashboard
  :after projectile
  :straight t
  :after all-the-icons
  ;; :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  ;; (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.emacs.d/sources/emacs.svg")
  (setq dashboard-banner-logo-title "Welcome to MageMacs, a magic GNU Emacs customization")
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
		          (projects . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-footer-messages '("Quod oculus non vidit, nec auris audivit - I Corinthios II,IX")))

(use-package transpose-frame
  :straight t)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :bind (:map copilot-mode-map
	      ("C-q <tab>" . copilot-accept-completion)
	      ("C-q <right>" . copilot-next-completion)
	      ("C-q <left>" . copilot-previous-completion)))
	      
(use-package candle-mode
  :after lsp-mode
  :straight (:host github :repo "PerplexSystems/candle-mode" :files ("dist" "*.el"))
  :hook ((candle-mode . lsp-deferred))
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (add-to-list 'lsp-language-id-configuration
		 '(candle-mode . "candle"))
  (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "millet-ls")
                      :activation-fn (lsp-activate-on "candle")
                      :server-id 'millet)))

(use-package erlang
  :straight t
  :hook
  (erlang-mode . lsp-deferred)
  (erlang-mode . yas-minor-mode))

;; (use-package eglot
;;   :straight t)

;; (use-package eldoc
;;   :straight t
;;   :hook (eglot-mode . eldoc-mode)
;;   :config
;;   (eldoc-add-command 'corfu-insert))

;; (use-package eldoc-box
;;   :straight t
;;   :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package lfe-mode
  :straight t
  :mode ("\\.lfe$" . lfe-mode)
  :hook
  (lfe-mode . display-line-numbers-mode)
  (lfe-mode . corfu-mode)
  (lfe-mode . eglot-ensure)
  (lfe-mode . yas-minor-mode))

;; (with-eval-after-load 'eglot
;;   (add-to-list
;;    'eglot-server-programs
;;    '(lfe-mode . ("~/Binaries/lfe-ls/_build/prod/bin/lfe-ls"
;;                  "--transport" "tcp" "--port" :autoport))))

(use-package org-drill
  :straight t)

(use-package sly
  :straight t
  :hook ((sly-mode . corfu-mode)))

(use-package cider
  :straight t
  :hook (cider-mode . corfu-mode))

(use-package clojure-mode
  :straight t
  :after cider
  :hook ((clojure-mode . lsp-deferred)))

(use-package rainbow-delimiters
  :straight t
  :hook
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lfe-mode . rainbow-delimiters-mode)
  (clojure-mode . rainbow-delimiters-mode))

(use-package smartparens
  :straight t
  :hook ((lisp-mode . smartparens-mode)
	 (emacs-lisp-mode . smartparens-mode)
	 (lfe-mode . smartparens-mode)
	 (clojure-mode . smartparens-mode)
	 (cider-mode . smartparens-mode))
  :bind (:map smartparens-mode-map
	      ("C-M-<right>" . 'sp-forward-sexp)
	      ("C-M-<left>" . 'sp-backward-sexp)
	      ("C-M-<down>" . 'sp-down-sexp)
	      ("C-M-<up>" . 'sp-up-sexp)
	      ("C-k" . 'sp-kill-sexp)
	      ("M-r" . 'sp-copy-sexp)
	      ("C-M-s" . 'sp-forward-slurp-sexp)
	      ("C-S-s" . 'sp-backward-slurp-sexp)
	      ("C-M-b" . 'sp-forward-barf-sexp)
	      ("C-S-b" . 'sp-backward-barf-sexp)))

(use-package corfu-terminal
  :straight t)

(use-package protobuf-mode
  :straight t)

(when (file-exists-p "~/.emacs.d/postgres-secrets.el")
  (load-file "~/.emacs.d/postgres-secrets.el"))

(use-package sqlformat
  :straight t
  :ensure t
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2" "-g")))

(lsp-register-client
  (make-lsp-client
   :new-connection
   (lsp-stdio-connection (list "swipl"
                               "-g" "use_module(library(lsp_server))."
                               "-g" "lsp_server:main"
                               "-t" "halt"
                               "--" "stdio"))
   :major-modes '(prolog-mode)
   :priority 1
   :multi-root t
   :server-id 'prolog-ls))

(use-package c++-mode
  :after lsp-mode
  :hook (c++-mode . lsp-deferred))

(use-package tuareg
  :straight t
  :hook (tuareg-mode . lsp-deferred))

(provide 'configuration)
;;; configuration.el ends here
