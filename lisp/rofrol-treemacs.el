;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs/27102429#27102429
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq-default smooth-scroll-margin 0)
(setq scroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position nil
      scroll-conservatively 100000)

(show-paren-mode t)


;;(require 'treemacs)

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

;;(tool-bar-mode -1)
;;(menu-bar-mode -1)
;; faster start: emacsclientw.exe -c -n -a "runemacs -fs --xrm \"Emacs.toolBar:0\" --xrm \"Emacs.menuBar:0\""

;;(defun toggle-bars ()
;;    (interactive)
;;    (toggle-menu-bar-mode-from-frame)
;;    (toggle-tool-bar-mode-from-frame))

;; works only once
;;(global-set-key [f7] 'toggle-bars)

;; works only once
;; https://unix.stackexchange.com/questions/7280/in-gnu-emacs-how-do-i-set-up-a-global-key-to-toggle-the-menu-bar/7291#7291
;;(defun toggle-menu-toolbar-modes ()
;;  (interactive)
;;  (tool-bar-mode (menu-bar-mode)))
;;(global-set-key (kbd "<f5>") 'toggle-menu-toolbar-modes)
(global-set-key (kbd "<f6>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<f7>") 'toggle-tool-bar-mode-from-frame)

(provide 'rofrol-display)
