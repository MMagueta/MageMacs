;;; mac-classic-theme.el --- Bringing back the glory days
;;
;;; Copyright (C) 2018  Free Software Foundation, Inc.
;;
;; Author: Andrew Hobson <>
;; Version: 0.0.1
;; Keywords: faces
;; URL: https://github.com/EricCrosson/mac-classic-theme
;; Package-Requires: ((emacs "24"))
;;
;; This file is not a part of GNU Emacs.
;;
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;;; Commentary:
;;
;; Provides a theme based on TextMate's Mac Classic theme.

;;; Code:

(deftheme mac-classic
  "Inspired by TextMate's Mac Classic theme.")

;;;###autoload
(custom-theme-set-faces
 'mac-classic
 '(default ((t (:background "white" :foreground "black"))))
 ;; Font lock faces
 '(font-lock-builtin-face            ((t (:foreground "#0000CD" :bold t))))
 '(font-lock-constant-face           ((t (:foreground "#C5060B" :bold t))))
 '(font-lock-preprocessor-face       ((t (:foreground "#3596A4"))))
 '(font-lock-keyword-face            ((t (:foreground "#0000FF" :bold t))))
 '(font-lock-type-face               ((t (:foreground "#585CF6" :bold t))))
 '(font-lock-variable-name-face      ((t (:foreground "#318495"))))
 '(font-lock-function-name-face      ((t (:foreground "#0000A2" :bold t))))
 '(font-lock-string-face             ((t (:foreground "#036A07"))))
 '(ruby-string-variable-face         ((t (:foreground "#26B31A"))))
 '(font-lock-comment-face            ((t (:foreground "#0066FF" :italic t))))
 '(font-lock-comment-delimiter-face  ((t (:foreground "#0066FF"))))
 '(whitespace-trailing               ((t (:background "#FFD0D0"))))
 '(font-lock-doc-face                ((t (:italic t :slant oblique :foreground "#B90690"))))
 '(font-lock-doc-string-face         ((t (:foreground "#B90690")))))
(provide-theme 'mac-classic)

(provide 'mac-classic-theme)

;;; mac-classic-theme.el ends here
