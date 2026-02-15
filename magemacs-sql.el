;;; magemacs-sql.el --- MageMacs SQL integration -*- lexical-binding: t; -*-

(with-eval-after-load 'sql
  (setq sql-postgres-options '("--no-psqlrc" "--pset=pager=off")))

(defun magemacs/lsp-sqls-from-connection-alist ()
  "Configure `lsp-sqls-connections` from `sql-connection-alist`."
  (require 'lsp-sqls)
  (setq lsp-sqls-connections
        (mapcar
         (lambda (conn)
           (let* ((props (cdr conn))
                  (driver "postgresql")
                  (server (cadr (assoc 'sql-server props)))
                  (port (cadr (assoc 'sql-port props)))
                  (user (cadr (assoc 'sql-user props)))
                  (db (cadr (assoc 'sql-database props))))
             `((driver . ,driver)
               (dataSourceName . ,(format "host=%s port=%s user=%s dbname=%s sslmode=disable"
                                          server port user db)))))
         sql-connection-alist))
  (message "Configured lsp-sqls-connections for %d databases"
           (length lsp-sqls-connections)))

(defun magemacs/lsp-sqls-use-connection ()
  "Configure `lsp-sqls` to use only the `psi` connection."
  (interactive)
  (require 'lsp-sqls)
  (let* ((connection-name (read-string "Connection name: "))
	 (conn (assoc (intern connection-name) sql-connection-alist)))
    (if conn
      (setq-local lsp-sqls-connections
                  (list
                   (let* ((props (cdr conn))
                          (driver "postgresql")
                          (server (cadr (assoc 'sql-server props)))
                          (port (cadr (assoc 'sql-port props)))
                          (user (cadr (assoc 'sql-user props)))
                          (db (cadr (assoc 'sql-database props))))
                     `((driver . ,driver)
                       (dataSourceName . ,(format "host=%s port=%s user=%s dbname=%s sslmode=require"
                                                  server port user db))))))
      (user-error "Could not load connection '%s'" connection-name)))
  (message "lsp-sqls configured to use the specified connection"))

(defun magemacs/use-sql-connections (var)
  "Replace `sql-connection-alist` with the value of VAR.
VAR should be a symbol referring to a variable that holds
an alist of SQL connection definitions."
  (interactive
   (list (intern
          (completing-read
           "Use SQL connections from variable: "
           (let (vars)
             (mapatoms
              (lambda (sym)
                (when (and (boundp sym)
			   (string-prefix-p "secret|" (symbol-name sym)))
                  (push (symbol-name sym) vars))))
             vars)
           nil t))))
  (if (and (boundp var)
           (listp (symbol-value var)))
      (progn
        (setq sql-connection-alist (symbol-value var))
        (message "sql-connection-alist replaced with value from %s" var))
    (user-error "Variable %s is not bound or not a list" var)))

(provide 'magemacs-sql)
;;; magemacs-sql.el ends here

