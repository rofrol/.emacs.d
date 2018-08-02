;; Some general settings
;; https://www.littlehart.net/atthekeyboard/2017/05/26/letting-emacs-into-your-grumpy-heart/
;; You can disable backups, but that's a bad idea for obvious reasons https://superuser.com/questions/236883/why-does-emacs-create-a-file-that-starts-with
(setq make-backup-files nil)
;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
(setq auto-save-default nil)

;; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
(delete-selection-mode t)

;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(electric-pair-mode t)

;; https://emacs.stackexchange.com/questions/12269/getting-electric-pair-mode-to-behave-more-nicely-around-existing-strings/14836#14836
(setq-default electric-pair-inhibit-predicate
              (lambda (c)
                (if (looking-at "\\([ \t]\\|$\\)")
                    (electric-pair-default-inhibit c)
                    t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'rofrol-system)
