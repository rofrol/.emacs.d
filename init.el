;; -*- lexical-binding: t; -*-

;; don't set your GC threshold so high, I advise against increasing it more than twice from the default.
;; https://www.reddit.com/r/emacs/comments/8rml8f/emacs_26_pixelscrollmode_gcs_like_theres_no/e0t89tf/
;; https://github.com/lewang/flx
;;(setq gc-cons-threshold 200000000)


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

;; required for validate-setq
(use-package validate
  :straight t
  :demand t)

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
(require 'rofrol-company)
(require 'rofrol-occur)
(require 'rofrol-elm)
(require 'rofrol-rust)
(require 'rofrol-side-windows)
(require 'rofrol-mouse)
(require 'rofrol-compile)
;;(require 'rofrol-fullscreen)
;;(require 'rofrol-electric)
;;(require 'rofrol-treemacs)
;;(require 'rofrol-git-bash)
;;(require 'rofrol-projectile)
;;(require 'rofrol-rest)
;;(require 'rofrol-xml-sgml)
