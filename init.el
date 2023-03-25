(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(load-file "~/.emacs.d/theme.el")
(load-file "~/.emacs.d/behaviour.el")
(load-file "~/.emacs.d/configuration.el")

