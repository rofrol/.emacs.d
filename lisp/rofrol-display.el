(set-frame-font "Source Code Pro-13" nil t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message "")

;;; http://ergoemacs.org/emacs/emacs_line_number_mode.html
;;; https://emacs.stackexchange.com/questions/36747/disable-line-numbers-in-helm-buffers-emacs-26
(when (version<= "26.0.50" emacs-version )
      ;;; enable in all programming modes
      (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(column-number-mode t)

;;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up/3017#3017
;;; https://www.emacswiki.org/emacs/FullScreen
;;; faster startup without resizing: emacs -fs
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(global-set-key [f11] 'toggle-frame-fullscreen)

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs/27102429#27102429
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq-default smooth-scroll-margin 0)
(setq scroll-step 1
      scroll-margin 3
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
        (treemacs)
        ;;; set focus to file
        ;;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
        (switch-to-buffer (other-buffer (current-buffer) 1))
        (global-set-key [f8] 'treemacs))

      (add-hook 'emacs-startup-hook 'init-treemacs)))

(provide 'rofrol-display)
