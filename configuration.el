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
  (defun magemacs/lsp-ui-doc-toggle ()
    "For some reason it is required to do at least once a call to lsp-ui-doc-show in order for this to work. Probably the problem resides on the frame created requiring some preparation before actually being used, so `frame-live-p` doesn't return nil."
    (interactive)
    (let ((frame (lsp-ui-doc--get-frame)))
      (if (frame-live-p frame)
	  (cond ((frame-visible-p frame) (lsp-ui-doc-hide))
		(t (lsp-ui-doc-show)))
	(message "Hover with the mouse or call `lsp-ui-doc-show` over some obect first. For why, read doc string."))))
  :bind
  (("s-?" . 'magemacs/lsp-ui-doc-toggle)))

(use-package nix-mode
  :straight t
  :hook
  (nix-mode . lsp-deferred)
  (nix-repl-mode . corfu-mode)
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
			(corfu-mode)
			(unless window-system (corfu-terminal-mode))))
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
  (if (seq-find (lambda (x) (or (eq 'marquardt x)
				(eq 'feng-shui x))) custom-enabled-themes)
      (set-face-attribute 'helm-selection nil
			  :background (color-lighten-name (face-attribute 'default :foreground) 50)
			  :foreground (color-darken-name (face-attribute 'default :background) 40))
    (set-face-attribute 'helm-selection nil
			:background (color-lighten-name (face-attribute 'default :foreground) 50)
			:foreground (color-darken-name (face-attribute 'default :background) 100)))
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
  ;; (setq dashboard-startup-banner "~/.emacs.d/sources/emacs.svg")
  (setq dashboard-startup-banner "~/.emacs.d/sources/magemacs.png")
  (setq dashboard-image-banner-max-height 300)
  (setq dashboard-image-banner-max-width 500)
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
  (lfe-mode . yas-minor-mode)
  :init
  (defvar lfe-mode-syntax-table
    (let ((table (copy-syntax-table lisp-mode-syntax-table)))
      ;; Like scheme we allow [ ... ] as alternate parentheses.
      (modify-syntax-entry ?\[ "(]  " table)
      (modify-syntax-entry ?\] ")[  " table)
      ;; ":" character should be used as punctuation to separate symbols
      (modify-syntax-entry ?: "." table)
      table)
    "Syntax table in use in Lisp Flavoured Erlang mode buffers.")
  (defun find-rebar-config-root ()
    "Find the nearest parent directory containing 'rebar.config'.
Starts from `default-directory`. Returns the path or nil if not found."
    (let ((dir (file-name-as-directory default-directory)))
      (catch 'found
(while dir
          (if (file-exists-p (expand-file-name "rebar.config" dir))
              (throw 'found dir)
            (let ((parent (file-name-directory (directory-file-name dir))))
              (if (or (null parent) (equal parent dir))
                  (setq dir nil) ;; reached root
(setq dir parent)))))
nil)))
  (defvar inferior-lfe-program
    ;; (concat "rebar3 --config " (find-rebar-config-root) "rebar.config lfe repl")
    ;; (setenv "REBAR_CONFIG" (concat (find-rebar-config-root)
    ;; "rebar.config"))
    ;; "rebar3 lfe repl")
    ;; "rebar3 --config /home/mmagueta/Projects/Nekoma/mahjong/server/rebar.config lfe repl"
    "rebar3")
  (defvar inferior-lfe-program-options '("lfe" "repl"))
  (defun inferior-lfe ()
    "Run an inferior LFE process, input and output via a buffer `*inferior-lfe*'.
Start the shell `inferior-lfe-program' `inferior-lfe-program-options' -env TERM vt100.
If a rebar project is found you are prompted (see `inferior-lfe-check-if-rebar-project')
and can choose to run lfe using rebar3."
    (interactive)
    (let ((prog inferior-lfe-program)
          (opts inferior-lfe-program-options)
          (rebar-project-root (inferior-lfe--is-rebar-project))
          (mix-project-root (inferior-lfe--is-mix-project)))
      (cond
       ((and inferior-lfe-check-if-rebar-project
             rebar-project-root
             (inferior-lfe--start-rebar-lfe))
(setq prog "sh"
              opts (list "-i" "-c" (concat "TERM=\"vt100\";"
                                           (format "cd %s" rebar-project-root)
                                           "; rebar3 lfe repl"))))
       ((and inferior-lfe-check-if-mix-project
             mix-project-root
             (inferior-lfe--start-mix-lfe))
(setq prog "sh"
              opts (list "-i" "-c" (concat "TERM=\"vt100\";"
                                           (format "cd %s" mix-project-root)
                                           "; mix lfe repl")))))
      (unless (comint-check-proc "*inferior-lfe*")
(set-buffer (apply (function make-comint)
                           "inferior-lfe" prog nil opts))
(inferior-lfe-mode))
      (setq inferior-lfe-buffer "*inferior-lfe*")
      (pop-to-buffer "*inferior-lfe*"))))

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   `(lfe-mode . (,(concat (getenv "HOME") "/Binary/lfe-ls/_build/prod/bin/lfe-ls")
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

;; (use-package pgmacs
  ;; :straight (:host github :repo "MMagueta/pgmacs" :branch "add-script-support" :files ("dist" "*.el")))

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package direnv
  :straight t)

(use-package projectile-direnv
  :straight t)

(use-package org-super-agenda
  :straight t
  :after org
  :custom
  (org-agenda-custom-commands
   '(("c" "Super Agenda"
      ((agenda "" ((org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :date today
                             :todo "TODAY")
                      (:name "Due soon"
                             :deadline future)
                      (:name "Overdue"
                             :deadline past)
                      (:name "Important"
                             :priority "A")))))))))
  (org-agenda-files `(,(concat (getenv "HOME") "/.emacs.d/agenda.org")))
  :config
  (org-super-agenda-mode))

(use-package typescript-mode
  :straight t)

(defun c-mode-compile-bind ()
  "Bind C-c C-c to `compile' in C mode."
  (local-set-key (kbd "C-c C-c") #'compile))

(add-hook 'c-mode-hook #'c-mode-compile-bind)
(add-hook 'c-mode-hook #'lsp-deferred)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("denon" "lsp"))
                  :activation-fn (lsp-activate-on "typescript")
                  :priority -1
                  :server-id 'ts-ls))

(use-package rust-mode
  :straight t)

(provide 'configuration)
;;; configuration.el ends here
