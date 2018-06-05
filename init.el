(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;; Modularization based on
;;; https://github.com/tonini/emacs.d
;;; https://www.reddit.com/r/emacs/comments/3q50do/best_way_organization_config_files_in_the_emacs/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'rofrol-system)
(require 'rofrol-display)
(require 'rofrol-utils)

(require 'company)
;;; required for company-elm
(add-hook 'after-init-hook 'global-company-mode)

(require 'elm-mode)
(with-eval-after-load 'elm-mode
     (add-to-list 'company-backends 'company-elm)
     (setq elm-format-on-save t))
