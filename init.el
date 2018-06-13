(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; Modularization based on
;;; https://github.com/tonini/emacs.d
;;; https://www.reddit.com/r/emacs/comments/3q50do/best_way_organization_config_files_in_the_emacs/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'rofrol-system)
(require 'rofrol-display)
(require 'rofrol-utils)

(use-package company
  :straight t
  :config
  (global-company-mode))

;; required for company-elm
;;(add-hook 'after-init-hook 'global-company-mode)

(use-package elm-mode
    :mode ("\\.elm\\'" . elm-mode)
    :straight t
    :init
    (progn
      (defun init-elm-mode ()
        "Disable electric-indent-mode and let indentation cycling feature work"
        (if (fboundp 'electric-indent-local-mode)
            (electric-indent-local-mode -1))
        (add-to-list 'company-backends 'company-elm)
	(setq elm-format-on-save t))

      (add-hook 'elm-mode-hook 'init-elm-mode)))

;; ripgrep
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-projectile.el
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-projectile.el
;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile/29200#29200
(use-package projectile
  :straight t
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
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
        (setq projectile-enable-caching nil))

      ;; FIXME: too slow while getting submodule files on Windows
      (setq projectile-git-submodule-command "")))

;; auto-switch to help buffer, then press q to close
;; https://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
;; https://stackoverflow.com/questions/1212426/how-do-i-close-an-automatically-opened-window-in-emacs
;; https://stackoverflow.com/questions/11106377/is-there-a-way-to-undo-split-window-below-in-emacs
(setq help-window-select t)
