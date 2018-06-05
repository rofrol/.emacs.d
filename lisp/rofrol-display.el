(set-frame-font "Source Code Pro-13" nil t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

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

(require 'treemacs)
(treemacs)
(global-set-key [f8] 'treemacs)

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs/27102429#27102429
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq-default smooth-scroll-margin 0)
(setq scroll-step 1
      scroll-margin 3
      scroll-preserve-screen-position t
      scroll-conservatively 100000)

(provide 'rofrol-display)
