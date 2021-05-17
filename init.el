;; ignore package cl deprecation warning
(setq byte-compile-warnings '(cl-functions))

(require 'org)
(org-babel-load-file
  (expand-file-name "settings.org" user-emacs-directory))
