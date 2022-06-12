

(defun efs/exwm-init-hook ()
  (exwm-workspace-switch-create 1)
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  (dashboard-refresh-buffer)
  (kill-buffer "*GNU Emacs*"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2))))

(use-package exwm
  :ensure t
  :config
  (setq exwm-workspace-number 5)
  
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
  
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)
  
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
  
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")
  
  ;; Automatically move EXWM buffer to current workspace when selected
  ;; (setq exwm-layout-show-all-buffers t)
  
  ;; Display all EXWM buffers in every workspace buffer list
  ;; (setq exwm-workspace-show-all-buffers t)
  
  ;; NOTE: Uncomment this option if you want to detach the minibuffer!
  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  ;;(setq exwm-workspace-minibuffer-position 'top)
  
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 1920x1080")
  
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 32)
  (exwm-systemtray-enable)
  
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space
  
  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)
	  
          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
	  
          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
	  
          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
	  
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  (use-package dmenu
    :ensure t
    :config
    (exwm-input-set-key (kbd "s-d") 'dmenu))
  
  (exwm-enable))
