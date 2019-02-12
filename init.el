;; -*- lexical-binding: t; -*-

;; https://github.com/lewang/flx
(setq gc-cons-threshold 200000000)


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

(setq load-prefer-newer t)

;; Modularization based on
;; https://github.com/tonini/emacs.d
;; https://www.reddit.com/r/emacs/comments/3q50do/best_way_organization_config_files_in_the_emacs/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'rofrol-system)
(require 'rofrol-display)
(require 'rofrol-utils)
(require 'rofrol-evil)
(require 'rofrol-scroll)
(provide 'rofrol-company)
(require 'rofrol-elm)
;;(require 'rofrol-fullscreen)
;;(require 'rofrol-electric)
;;(require 'rofrol-treemacs)
;;(require 'rofrol-git-bash)
;;(require 'rofrol-projectile)
;;(require 'rofrol-rest)
