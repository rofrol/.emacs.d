;; -*- lexical-binding: t; -*-

(use-package rust-mode
    :mode ("\\.lalrpop\\'" . elm-mode)
    :straight t
    :config
	(setq rust-format-on-save t))

(provide 'rofrol-rust)
