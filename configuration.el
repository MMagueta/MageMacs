(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-lens-mode)
	 ;; Solves F# buffer out of sync
	 (lsp-mode . (lambda () (lsp-treemacs-sync-mode -1)))))

(use-package lsp-ui
  :straight t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 7)
  (setq lsp-ui-sideline-diagnostic-max-line-length 20)
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

(use-package rainbow-delimiters
  :straight t
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

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
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
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
	   (corfu-auto-delay 0.25)
	   (corfu-min-width 15)
	   (corfu-max-width 70)
	   (corfu-popupinfo-delay corfu-auto-delay))
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
   (setq compile-command "dotnet watch run")
   (setq inferior-fsharp-program "dotnet fsi"))

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
  ;; (setq dashboard-startup-banner "/Users/mmagueta/.emacs.d/sources/emacs.svg")
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
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
	      ("C-q <tab>" . copilot-accept-completion)
	      ("C-q <right>" . copilot-next-completion)
	      ("C-q <left>" . copilot-previous-completion)))

(provide 'configuration)
;;; configuration.el ends here
