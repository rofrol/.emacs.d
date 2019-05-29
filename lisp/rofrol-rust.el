;; -*- lexical-binding: t; -*-

(use-package rust-mode
    :mode ("\\.lalrpop\\'" . elm-mode)
    :straight t
    :config
    (setq rust-format-on-save nil)
    (defun rust-mode-format-buffer ()
      "format rust code without jumping to beginning when doing undo"
      "https://github.com/rust-lang/rust-mode/issues/162#issuecomment-497137206"
      (interactive)
      (shell-command
       (format "rustfmt %s"
	       (shell-quote-argument (buffer-file-name))))
      (revert-buffer t t t))

    (add-hook 'rust-mode-hook
	      (lambda ()
		(add-hook 'after-save-hook #'rust-mode-format-buffer nil 'make-it-local))))

(provide 'rofrol-rust)
