;; Needs restart
;; https://stackoverflow.com/questions/28428382/how-to-manage-fonts-in-emacs
;; https://emacs.stackexchange.com/questions/2501/how-can-i-set-default-font-in-emacs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;; https://stackoverflow.com/questions/534307/set-emacs-defaut-font-face-per-buffer-mode
;; `M-x describe-face` will get information like `Face: markdown-pre-face`
;; https://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/
;; https://superuser.com/questions/422968/how-to-find-out-current-font-used-in-my-emacs
;;(set-frame-font "Source Code Pro-13" nil t)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)
(copy-face 'default 'fixed-pitch)

;; Don't let Emacs hurt your ears
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message "")

;; http://ergoemacs.org/emacs/emacs_line_number_mode.html
;; https://emacs.stackexchange.com/questions/36747/disable-line-numbers-in-helm-buffers-emacs-26
(when (version<= "26.0.50" emacs-version )
      ;;; enable in all programming modes
      (defun init-line-numbers-mode ()
        (display-line-numbers-mode)
    (setq display-line-numbers-width-start t))
      (add-hook 'prog-mode-hook 'init-line-numbers-mode))

(column-number-mode t)

;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up/3017#3017
;; https://www.emacswiki.org/emacs/FullScreen
;; faster startup without resizing: emacs -fs
;; if you want maximized, don't use -mm on Windows
;; looks like toggle-frame-maximized is faster than default-frame-alist
;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up/3000#3000
(toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(defun toggle-fullscreen-maximize ()
  (interactive)
  (toggle-frame-maximized)
  (toggle-frame-fullscreen))
(global-set-key [f11] 'toggle-fullscreen-maximize)

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

;; https://emacs.stackexchange.com/questions/392/how-to-change-the-cursor-type-and-color
;; (setq-default cursor-type 'bar)
(setq-default cursor-type 'hollow)

(provide 'rofrol-display)
