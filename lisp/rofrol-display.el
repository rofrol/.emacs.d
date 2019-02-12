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

(tool-bar-mode -1)
(menu-bar-mode -1)
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
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Cursor-Display.html
;; (setq-default cursor-type 'bar)
(setq-default cursor-type '(bar . 2))
;; (setq-default cursor-type 'hollow)

;; show border around zero width space ​ https://news.ycombinator.com/item?id=16754256
;; to insert: `C-x 8 RET` then search for ZERO WIDTH SPACE
;; or `C-q 20013 RET'
(update-glyphless-char-display 'glyphless-char-display-control '((format-control . empty-box) (no-font . hex-code)))

(provide 'rofrol-display)
