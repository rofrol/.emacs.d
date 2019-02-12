;; https://www.reddit.com/r/emacs/comments/726p7i/evil_mode_and_use_package/dnh3338/
;; https://github.com/raxod502/straight.el/issues/250
;; https://www.linode.com/docs/tools-reference/tools/emacs-evil-mode/
;; https://github.com/noctuid/evil-guide
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://www.emacswiki.org/emacs/Evil
;; https://www.reddit.com/r/emacs/comments/2mcj85/switching_from_vim_should_i_use_emacs_evil_or/
;; C-z -> evil-normal-mode
(use-package evil
  :straight t
  :init ;; tweak evil's configuration before loading it
  ;; (setq evil-search-module 'evil-search)
  ;; (setq evil-ex-complete-emacs-commands nil)
  ;; (setq evil-vsplit-window-right t)
  ;; (setq evil-split-window-below t)
  ;; (setq evil-shift-round nil)
  ;; (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t) 
  :config ;; tweak evil after loading it
  (evil-mode))

;; https://github.com/syl20bnr/spacemacs/issues/9330#issuecomment-319448299
;; http://wikemacs.org/wiki/Evil
(use-package evil-escape
 :straight t
 :after evil
 :commands evil-escape-mode
 :init
 (setq evil-escape-key-sequence "jk"
       evil-escape-delay 0.25)
 (evil-escape-mode)
 :config
   (define-key evil-normal-state-map "\C-y" 'yank)
   (define-key evil-insert-state-map "\C-y" 'yank)
   (define-key evil-visual-state-map "\C-y" 'yank)
   (define-key evil-insert-state-map "\C-e" 'end-of-line)
   (define-key evil-normal-state-map "\C-w" 'evil-delete)
   (define-key evil-insert-state-map "\C-w" 'evil-delete)
   (define-key evil-insert-state-map "\C-r" 'search-backward)
   (define-key evil-visual-state-map "\C-w" 'evil-delete))

(provide 'rofrol-evil)
