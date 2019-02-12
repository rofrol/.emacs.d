;; -*- lexical-binding: t; -*-

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

(provide 'rofrol-fullscreen)
