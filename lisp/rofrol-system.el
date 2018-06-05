;;; Some general settings
;;; https://www.littlehart.net/atthekeyboard/2017/05/26/letting-emacs-into-your-grumpy-heart/
(setq make-backup-files nil)
(define-coding-system-alias 'UTF-8 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)

(provide 'rofrol-system)
