;;; ob-fsharp.el --- org-babel functions for fsharp evaluation

;; Copyright (C) 2016 Feng Zhou

;; Author: Feng Zhou <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-fsharp
;; Keywords: org babel fsharp
;; Version: 0.0.1
;; Created: 10th Apr 2016
;; Package-Requires: ((org "8") (emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-babel functions for fsharp evaluation
;;

;;; Code:
(require 'ob)
(require 'subr-x)

(defvar ob-fsharp-process-output "")

(defvar ob-fsharp-eoe "ob-fsharp-eoe")

(defun org-babel-execute:fsharp (body params)
  (let ((session (cdr (assoc :session params))))
    (ob-fsharp--ensure-session session)
    (ob-fsharp-eval-in-repl session body)))

(defun ob-fsharp--ensure-session (session)
  (let ((name (format "*ob-fsharp-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (let ((process (with-current-buffer (get-buffer-create name)
                       (start-process name name "dotnet" "run"))));;"dotnet" "fsi" "--nologo" "--readline-"))))
        (sit-for 1)
        (set-process-filter process 'ob-fsharp--process-filter)))))

(defun ob-fsharp--process-filter (process output)
  (setq ob-fsharp-process-output (concat ob-fsharp-process-output output)))

(defun ob-fsharp--wait (pattern)
  (while (not (string-match-p pattern ob-fsharp-process-output))
    (sit-for 0.2)))

(defun ob-fsharp-eval-in-repl (session body)
  (let ((name (format "*ob-fsharp-%s*" session)))
    (setq ob-fsharp-process-output "")
    (process-send-string name (format "%s;;\n\"%s\";;\n" body ob-fsharp-eoe))
    (accept-process-output (get-process name) nil nil 1)
    (ob-fsharp--wait ob-fsharp-eoe)
    (string-trim
     (replace-regexp-in-string
      (format "^> val it : string = \"%s\"\n> " ob-fsharp-eoe) "" ob-fsharp-process-output))))


(provide 'ob-fsharp)
;;; ob-fsharp.el ends here
