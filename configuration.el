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
  :after lsp-mode
  :bind (("C-c M-n" . dap-next)
         ("C-c M-s" . dap-step-in)
         ("C-c M-a" . dap-step-out)
         ("C-c M-w" . dap-continue)))

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
  (unless (eq system-type 'darwin)
    (defun nix-repl-with-variable ()
      (interactive)
      (let ((variables (read-string "Nix repl variable to load: ")))
	(defcustom nix-repl-executable-args `("repl" ,variables)
	  "Arguments to provide to nix-repl."
	  :type '(repeat string))
	(nix-repl))))
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
  :hook ((prog-mode . (lambda ()
			(display-line-numbers-mode)
			(setq display-line-numbers 'relative)))))

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
  :hook ((prog-mode . (lambda ()
			(corfu-mode)
			(unless window-system (corfu-terminal-mode))))
	 (sql-mode . (lambda ()
			(corfu-mode -1)
			(unless window-system (corfu-terminal-mode -1))))
	 (corfu-mode . corfu-popupinfo-mode)
	 (eshell-mode . corfu-mode)))

(use-package corfu-terminal
  :straight t)

(use-package fsharp-mode
  :straight t
  :after lsp-mode
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
  ;; (setq lsp-fsharp-server-install-dir "/Users/mmagueta/.dotnet/tools/")
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
;;   :straight t
;;   :bind (:map eglot-mode-map
;; 	      ("C-c C-d" . eglot-help-at-point)
;; 	      ("C-c C-r" . eglot-code-actions)))

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

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '(lfe-mode . ("/User/mmagueta/Binary/lfe-ls/_build/prod/bin/lfe-ls"
                 "--transport" "tcp" "--port" :autoport))))

(use-package org-drill
  :straight t)

(use-package sly
  :straight t
  :hook ((sly-mode . corfu-mode)
	 (sly-mode . smartparens-mode))
  :init
  ;; (setq sly-lisp-implementations
	;; '((sbcl ("/etc/profiles/per-user/mmagueta/bin/sbcl"))
  ;; (ecl ("/etc/profiles/per-user/mmagueta/bin/ecl"))))
  (defun sly-eval-last-expression-eros ()
    (interactive)
    (cl-destructuring-bind (output value)
	(sly-eval `(swank:eval-and-grab-output ,(sly-last-expression)))
      (eros--make-result-overlay (concat output value)
	:where (point)
	:duration eros-eval-result-duration)))
  :bind (:map sly-mode-map
	      ("C-x C-e" . sly-eval-last-expression-eros)))

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

(use-package protobuf-mode
  :straight t)

(let ((secrets (concat (getenv "HOME") "/.emacs.d/secrets.el")))
  (when (file-exists-p secrets)
    (load-file secrets)))

;; (with-eval-after-load 'sql-mode
;;   (progn
;;     (add-to-list 'sql-connection-alist
;; 		 '(supabase-local (sql-product 'postgres)
;; 				  (sql-port 54322)
;; 				  (sql-user    "postgres")
;; 				  (sql-server "localhost")
;; 				  (sql-database   "postgres")))
;;     (add-to-list 'sql-connection-alist
;; 		 '(dev (sql-product 'postgres)
;; 		       (sql-port 5433)
;; 		       (sql-user    "admin")
;; 		       (sql-server "localhost")
;; 		       (sql-database   "supabase")))
;;     (add-to-list 'sql-connection-alist
;; 		 '(supabase-prod (sql-product  'postgres)
;; 				 (sql-port     6543)
;; 				 (sql-user     "postgres.lzrwqzryybmmzdjyrxtf")
;; 				 (sql-server   "aws-0-us-east-1.pooler.supabase.com")
;; 				 (sql-database "postgres")))
;;     (setq lsp-sqls-workspace-config-path nil)
;;     ;; (setq lsp-sqls-connections
;; 	  ;; '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=54322 user=postgres password=postgres dbname=postgres sslmode=disable"))))
;;     ;; (setq lsp-sqls-connections
;; 	  ;; '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5433 user=admin password=admin dbname=supabase sslmode=disable"))))
;;     ))

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
  :after dap-mode
  :hook (tuareg-mode . lsp-deferred)
  :config
  (require 'dap-mode)
  (require 'dap-codelldb)
  (require 'dap-ocaml))

(use-package eros
  :straight t
  :hook (emacs-lisp-mode . eros-mode))

(use-package pgmacs
  :straight (:host github :repo "MMagueta/pgmacs" :branch "add-script-support" :files ("dist" "*.el")))

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package direnv
  :straight t)

(use-package projectile-direnv
  :straight t)

(provide 'configuration)
;;; configuration.el ends here
