;; -*- lexical-binding: t; -*-

;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-projectile.el
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-projectile.el
;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile/29200#29200
(use-package projectile
  :straight t
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
    ;; use rg for find-file
    (let ((command
           (cond
            ((executable-find "rg")
             (let ((rg-cmd ""))
               (dolist (dir projectile-globally-ignored-directories)
                 (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
               (concat "rg -0 --files --color=never --hidden" rg-cmd))))))
    (setq projectile-generic-command command))

    ;; Faster searching on Windows
    (when (eq system-type 'windows-nt)
      (when (or (executable-find "rg") (executable-find "pt") (executable-find "ag"))
        (setq projectile-indexing-method 'alien)
        (setq projectile-enable-caching t))

      ;; FIXME: too slow while getting submodule files on Windows
      (setq projectile-git-submodule-command ""))
      ;; https://emacs.stackexchange.com/questions/10465/turn-on-projectile-mode-only-for-files-in-actual-projects
      ;; https://gist.github.com/hlissner/f80647f7a390bfe78a805a40b9c28e9b
      ;; https://emacs.stackexchange.com/questions/38759/projectile-buffer-names-with-project-relative-filenames
      (setq projectile-mode-line
	    '(:eval (if (projectile-project-p)
			(format " Projectile[%s]"
				(concat (projectile-project-name) "/"
					(if (eq nil buffer-file-name)
					    ""
					  (f-dirname (f-relative buffer-file-name (projectile-project-root))))))
		      ""))))


;; https://emacs.stackexchange.com/questions/35432/how-to-set-projectile-project-name-as-frame-title
;; https://github.com/syl20bnr/spacemacs/issues/2139
;; Set my-projectile-project-name to projectile-project-name,
;; so that later I can also set projectile project name when in *Messages*
(defun my-projectile-switch-project-action ()
  (set-frame-parameter nil 'my-projectile-project-name projectile-project-name)
  (projectile-run-eshell)
  (projectile-find-file))
  ;; (ivy-switch-buffer))

(setq projectile-switch-project-action 'my-projectile-switch-project-action)

(setq frame-title-format
    '(""
      "%b"
      (:eval
          (if (projectile-project-p)
              (let ((project-name (projectile-project-name)))
                  (if (not (string= "-" project-name))
                    (format " in [%s]" (concat (project-name) "/"
					(if (eq nil buffer-file-name)
					    ""
					  (f-dirname (f-relative buffer-file-name (projectile-project-root))))))
                    (format " in [%s]" (frame-parameter nil 'my-projectile-project-name))))
	      ""))))

;; (use-package counsel-projectile
;;    :after (counsel projectile)
;;    :straight t
;;    :defer t)
;;

(provide 'rofrol-projectile)
