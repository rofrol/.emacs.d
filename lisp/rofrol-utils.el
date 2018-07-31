(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))

(defun reload-user-init-file()
  (interactive)
  (load-file user-init-file))

;; this doesn't create dirs:
;; https://rejeep.github.io/emacs/elisp/2010/03/26/rename-file-and-buffer-in-emacs.html
;; https://stackoverflow.com/questions/17829619/rename-current-buffer-and-related-file-in-emacs#comment76700497_17834957
;; old crux version https://stackoverflow.com/questions/17829619/rename-current-buffer-and-related-file-in-emacs/17834957#17834957

(provide 'rofrol-utils)
