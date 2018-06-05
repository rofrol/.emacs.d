(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))

(defun reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(provide 'rofrol-utils)
