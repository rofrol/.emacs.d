;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hyperlinking.html
;; this is better than browse-url-at-mouse, because only hyperlinks can be clicked, not sth like some.text
;; https://emacs.stackexchange.com/questions/30521/rendering-urls-as-clickable-links/30522#30522
;; there is also goto-address-mode https://emacs.stackexchange.com/questions/27094/how-to-make-hyperlinks-clickable-in-markdown-mode/27100#27100
;; which will make hyperlinks clickable
;; which can be activated for certain modes `(add-hook 'mh-show-mode-hook 'goto-address)` or `M-x goto-address`
;; no need for below settings in emacs-26 or earlier
;;(global-set-key [C-down-mouse-1] 'ffap-at-mouse)

;; https://askubuntu.com/questions/596143/emacs-keeps-popping-up-buffer-menu-when-i-dont-want-it
(define-key global-map (kbd "<C-mouse-1>") 'ignore)

;; Ctrl click a link, also disables mouse-buffer-menu
;; https://www.emacswiki.org/emacs/BrowseUrl
;;(global-set-key [C-down-mouse-1] 'browse-url-at-mouse)

(provide 'rofrol-mouse)
