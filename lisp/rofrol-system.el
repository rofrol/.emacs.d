;; Some general settings
;; https://www.littlehart.net/atthekeyboard/2017/05/26/letting-emacs-into-your-grumpy-heart/
(setq make-backup-files nil)
;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
(setq auto-save-default nil)
;; https://www.reddit.com/r/emacs/comments/29zm3q/how_to_get_rid_of_filename_files_that_emacs_is/
(setq create-lockfiles nil)
(define-coding-system-alias 'UTF-8 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)

;; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
(delete-selection-mode t)

;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(electric-pair-mode t)

(provide 'rofrol-system)
