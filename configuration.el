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

(use-package flycheck-elsa
  :ensure t
  :hook
  ((emacs-lisp-mode . flycheck-mode)
   (emacs-lisp-mode . flymake-mode)))

(use-package flycheck
  :ensure t
  :config
  (use-package flymake-flycheck
    :ensure t))

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-lens-mode)
	 ;; Solves F# buffer out of sync
	 (lsp-mode . (lambda () (progn
				  (ws-butler-mode -1)
				  (lsp-treemacs-sync-mode -1)))))
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
  :hook
  (scala-mode . lsp-deferred)
  (scala-mode . yas-minor-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

;; (add-hook 'c-or-c++-mode #'(lambda () (lsp)))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . lsp-deferred))
  :custom
  (org-babel-clojure-backend 'cider)
  :init
  (use-package cider
    :ensure t
    :hook ((cider-repl-mode . corfu-mode))))

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
  :custom (sbt:program-options '("-Dsbt.supershell=false")))

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
  (nix-mode . lsp-deferred)
  ;; (nix-repl-mode . company-mode)
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
  (haskell-mode . lsp-deferred)
  (haskell-mode . yas-minor-mode))

(use-package tuareg
  :ensure t
  :hook (tuareg-mode . lsp-deferred)
  :config
  (setq tuareg-match-patterns-aligned t)
  (setq tuareg-indent-align-with-first-arg nil))

(use-package swiper
  :ensure t
  :bind
  ("\C-s" . swiper))
   
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
  :ensure t
  :hook ((sly-mode . corfu-mode)))

;; (use-package linum-relative
;;   :ensure t
;;   :hook ((prog-mode . linum-relative-mode)))

(use-package display-line-numbers
  :ensure t
  :hook ((prog-mode . (lambda () (progn
				   (display-line-numbers-mode)
				   (setq display-line-numbers 'relative))))))

;; (use-package company-quickhelp
;;    :ensure t
;;    :config
;;    (defun load-company-face ()
;;      (require 'color)
;;      (setq company-tooltip-limit 10
;; 	   company-tooltip-flip-when-above t
;; 	   company-tooltip-maximum-width 70
;; 	   company-tooltip-minimum-width 15
;; 	   company-quickhelp-color-foreground (color-lighten-name (face-attribute 'default :foreground) 10)
;; 	   company-quickhelp-color-background (color-lighten-name (face-attribute 'default :background) 10)
;; 	   pos-tip-foreground-color (face-attribute 'default :foreground) ; set pos-tip font color to the same as the theme
;; 	   company-tooltip-align-annotations t ; align annotations to the right tooltip border
;; 	   company-quickhelp-delay '0.25
;; 	   company-quickhelp-use-propertized-text t)
;;      (let ((bg (face-attribute 'default :background)))
;;        (custom-set-faces
;; 	`(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 10)))))
;; 	`(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;; 	`(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 10)))))
;; 	`(company-tooltip-selection ((t (:inherit highlight;font-lock-function-name-face
;; 						  ))))
;; 	`(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))
;;    (add-hook 'company-mode-hook 'load-company-face))
   ;; :init (company-quickhelp-mode t)
   ;; :hook
   ;; ((emacs-lisp-mode . (lambda () (company-mode)))
    ;; (company-mode . load-company-face)))

(use-package corfu
  :ensure t
  :custom ((corfu-auto t)
	   (corfu-auto-delay 0.25)
	   (corfu-min-width 15)
	   (corfu-max-width 70)
	   (corfu-popupinfo-delay corfu-auto-delay))
  :hook ((prog-mode . corfu-mode)
	 (corfu-mode . corfu-popupinfo-mode)
	 (eshell-mode . corfu-mode)
	 (lsp-mode . (lambda () (progn
				  (company-mode nil)
				  (corfu-mode))))))

;; (use-package marginalia
;;   :ensure t
;;   :bind
;;   (("M-A" . marginalia-cycle)
;;    :map minibuffer-local-map
;;    ("M-A" . marginalia-cycle)))

;; (use-package fstar-mode
;;   :ensure t)

(use-package fsharp-mode
   :ensure t
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
   (setq compile-command "dotnet watch run")
   (setq inferior-fsharp-program "dotnet fsi"))
   
(use-package csharp-mode
   :ensure t
   :hook ((csharp-mode . lsp-deferred)))

(use-package magit
   :ensure t
   :bind
   ("C-x g" . magit-status)
   :config
   (use-package diff-hl
     :ensure t))

(use-package helm
   :ensure t
   :init
   (helm-mode t)
   (set-face-attribute 'helm-selection nil
		       :background (color-lighten-name (face-attribute 'default :foreground) 50)
		       :foreground (color-darken-name (face-attribute 'default :background) 100))
   :bind
   ("M-x" . helm-M-x)
   ("C-x b" . helm-buffers-list))

(use-package windmove
  :ensure t
  :bind
  ("C-c <right>" . windmove-right)
  ("C-c <left>" . windmove-left)
  ("C-c <up>" . windmove-up)
  ("C-c <down>" . windmove-down))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-d" . mc/mark-next-like-this-word)
  ("C-c m c" . mc/edit-lines)
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this))

(use-package move-text
  :ensure t
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))

(use-package eshell-syntax-highlighting
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode t))

(use-package org
  :ensure t
  :custom
  (org-log-done 'time)
  (org-confirm-babel-evaluate nil)
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  ;; make some org commands available from anywhere (not only org mode)
  (global-set-key (kbd "C-c c") 'org-capture))

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
  :custom
  (org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(use-package projectile
  :ensure t
  :init
  (projectile-mode t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package dashboard
  :after projectile
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

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(use-package perspective
  :ensure t
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (persp-mode)
  :custom (persp-suppress-no-prefix-key-warning t))

(use-package org-sql
  :ensure t
  :config
  (use-package ob-sql-mode
    :ensure t)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((sql . t))))

(use-package typescript-mode
  :ensure t
  :hook ((typescript-mode . lsp-mode)))

(use-package lsp-mssql
  :ensure t
  :custom (lsp-mssql-connections
	   [(:server "localhost"
		     :port 1433
                     :database "msdb"
                     :user "sa"
                     :password "P@ssw0rd")])
  :bind (("C-c r" . lsp-mssql-execute-region)))

(provide 'configuration)
;;; configuration.el ends here
