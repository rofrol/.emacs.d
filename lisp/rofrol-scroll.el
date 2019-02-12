;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs/27102429#27102429
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ; three lines at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq-default smooth-scroll-margin 0)
(setq scroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position nil
      scroll-conservatively 100000)

;; https://emacs.stackexchange.com/questions/31402/how-to-avoid-scrolling-with-large-files-hanging-for-short-periods-of-time-hold
(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)

;; https://unix.stackexchange.com/questions/252995/how-can-mouse-support-be-enabled-in-terminal-emacs/406519#406519
;; https://dwm.suckless.narkive.com/Vf392Hxa/dev-st-how-can-i-scroll-up-in-emacs
(xterm-mouse-mode 1)

;; https://emacs.stackexchange.com/questions/41441/can-scrolling-be-smoother-than-single-line-in-emacs-org-mode
(pixel-scroll-mode 1)

(provide 'rofrol-scroll)
