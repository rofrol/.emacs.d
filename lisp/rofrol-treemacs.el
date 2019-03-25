;; -*- lexical-binding: t; -*-

(use-package treemacs
    :straight t
    :config
        (setq treemacs-is-never-other-window nil
              treemacs-position 'right
              treemacs-follow-mode t)
    :init
    (progn
      (defun init-treemacs ()
        ;;(treemacs)
        ;;; set focus to file
        ;;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
        (switch-to-buffer (other-buffer (current-buffer) 1))
        (global-set-key [f8] 'treemacs))

      (add-hook 'emacs-startup-hook 'init-treemacs)))

(provide 'rofrol-treemacs)
