;; -*- lexical-binding: t; -*-

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :straight t
  :config (progn
	    ;; https://github.com/bdkoepke/dotfiles/blob/4c50ec5e414afb4ee27e5d17aa39432e2e988018/emacs.lisp#L67
	    ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
	    ;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
	    ;; (add-hook 'js-mode-hook 'js2-minor-mode)
	    ;; also https://dougie.io/emacs/indentation/
	    (setq-default indent-tabs-mode nil)

	    (add-hook 'js2-mode-hook
		      (lambda ()
			(ws-tabs)
			)
		      )
	    )
  )

(provide 'rofrol-js2-mode)
