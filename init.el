(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
;;; re-defining package--save-selected-packages to do nothing
;;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/d7yt2yu/
(defun package--save-selected-packages (&rest opt) nil)

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
