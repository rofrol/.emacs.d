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

;; https://github.com/bbatsov/crux/blob/c79985f69b7cd96edb505199bd751f71ce6d4e58/crux.el#L330
;; there is bug cannot be called repeatedly https://github.com/bbatsov/crux/issues/42 but I can run it repeatedly
(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(provide 'rofrol-utils)
