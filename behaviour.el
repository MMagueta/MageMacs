(require 'use-package)

(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "~/.saves")))

(defun emacs-lisp/eval-entire-expression ()
  (interactive)
  (forward-sentence)
  (minibuffer-message (eval-last-sexp (sexp-at-point))))

(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'emacs-lisp/eval-entire-expression)
(global-set-key (kbd "s-e") 'eshell)
(global-set-key (kbd "s-t") 'transpose-frame)

(use-package comint
  :after fsharp-mode
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
  (add-hook 'kill-buffer-hook 'comint-write-input-ring)
  :hook
  (inferior-fsharp-mode . turn-on-comint-history))

(provide 'behaviour)
