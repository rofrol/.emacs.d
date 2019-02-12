;; -*- lexical-binding: t; -*-

;; Some general settings
;; https://www.littlehart.net/atthekeyboard/2017/05/26/letting-emacs-into-your-grumpy-heart/
;; You can disable backups, but that's a bad idea for obvious reasons https://superuser.com/questions/236883/why-does-emacs-create-a-file-that-starts-with
(setq make-backup-files nil)
;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
(setq auto-save-default nil)

;; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
(delete-selection-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; https://www.reddit.com/r/emacs/comments/29zm3q/how_to_get_rid_of_filename_files_that_emacs_is/
;; https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
(setq create-lockfiles nil)
(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
;; https://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs
;; https://emacs.stackexchange.com/questions/22727/pasting-text-from-clipboard-why-m-instead-of-linebreaks
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-16-le)) ;; to paste i.e. ' instead of \222

(setq confirm-kill-emacs 'y-or-n-p)

;; No need as now C-g closes side window
;; auto-switch to help buffer, then press q to close
;; https://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
;; https://stackoverflow.com/questions/1212426/how-do-i-close-an-automatically-opened-window-in-emacs
;; https://stackoverflow.com/questions/11106377/is-there-a-way-to-undo-split-window-below-in-emacs
;;(setq help-window-select t)

;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; https://emacs.stackexchange.com/questions/22266/backspace-without-adding-to-kill-ring
;; https://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
;; http://ergoemacs.org/emacs/emacs_kill-ring.html
;; https://stackoverflow.com/questions/16915712/implementation-of-a-kill-word-or-line-function-in-emacs
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key [M-backspace] 'backward-delete-word)

(use-package crux
  :straight t
  ;;:init (add-hook 'after-init-hook #'projectile-mode)
  :config
  ;; also http://ergoemacs.org/emacs/rename_file_pattern.html
  ;; also http://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
  ;; also https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
  ;; also https://www.emacswiki.org/emacs/DuplicateLines
  ;; https://github.com/bbatsov/crux/blob/c79985f69b7cd96edb505199bd751f71ce6d4e58/crux.el#L330
  ;; there is bug cannot be called repeatedly https://github.com/bbatsov/crux/issues/42 but I can run it repeatedly
  (global-set-key (kbd "C-c r") 'crux-rename-file-and-buffer)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region))


;; alternative https://github.com/bbatsov/super-save
(save-place-mode 1)

;; https://emacs.stackexchange.com/questions/13941/move-selected-lines-up-and-down
;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(use-package drag-stuff
  :straight t
  :bind (("M-<up>" . 'drag-stuff-up)
         ("M-<down>" . 'drag-stuff-down)))

;; https://superuser.com/questions/397806/emacs-modify-quit-window-to-delete-buffer-not-just-bury-it
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;;(defun quit-window--kill (&optional ARG PRED) 
;;  "Kill window when quit-window"
;;  (ad-set-arg 0 t))
;;
;;(advice-add 'quit-window :before #'quit-window--kill)

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;; disabling, because does not work good in elm-mode
;; Map to C-j to have electric-newline-and-maybe-indent on Enter
;; https://www.reddit.com/r/emacs/comments/4pxvhk/mj_to_ret_when_typing_a_comment/
;; (global-set-key (kbd "RET") (kbd "C-j"))


;; matching whitespace literally, toggle with `M-s SPC`
;; otherwise insert space literally `C-q SPC` or `M-x isearch-toggle-lax-whitespace`
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lax-Search.html#Lax-Search
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Special-Isearch.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexp-Search.html
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Controlling-case-sensitivity.html
;; https://emacs.stackexchange.com/questions/2819/match-two-spaces-with-incremental-search
(set-variable 'search-whitespace-regexp nil)

;; enable search case sensitivity
;; `M-c` to toogle, `C-s` then `M-e` to edit last search and `C-s`
;; https://stackoverflow.com/questions/22687635/emacs-24-3-case-sensitive-search-fails-with-c-s-c-s-search-again
;; https://www.emacswiki.org/emacs/CaseFoldSearch
(setq-default case-fold-search nil)

;; enable lexical-binding in scratch buffer on start up
(add-hook 'lisp-interaction-mode-hook
      (lambda ()
         (setq lexical-binding t)))

;; Disabling because of error: Symbol's value as variable is void: rectangle-mark-mode-map
;; https://emacs.stackexchange.com/questions/39414/immediately-invoke-string-rectangle-upon-rectangle-mark-mode-selection/42597#42597
;; (defun string-rectangle-with-initial (char)
;;   (interactive (list last-input-event))
;;   (push char unread-command-events)
;;   (call-interactively 'string-rectangle))

;; (define-key rectangle-mark-mode-map
;;   [remap ] 'string-rectangle-with-initial)

;; https://stackoverflow.com/questions/13965966/unset-key-binding-in-emacs
(global-unset-key (kbd "C-v"))

;; https://www.emacswiki.org/emacs/KillBufferUnconditionally
;; https://stackoverflow.com/questions/6467002/how-to-kill-buffer-in-emacs-without-answering-confirmation
;; https://superuser.com/questions/632750/how-can-i-eliminate-prompts-and-dialog-boxes-in-emacs-and-enable-auto-saving
;; https://emacs.stackexchange.com/questions/3245/kill-buffer-prompt-with-option-to-diff-the-changes
;; https://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; C-; does not work with evil
;;(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "<f4>") 'comment-or-uncomment-region-or-line)

;; does not move cursor to next line
;; https://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line/9697222#9697222
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end move)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position) move t))
        (comment-or-uncomment-region beg end)
	;; I don't know why but this code prevents from unselecting after commenting region
        (if (move)
            ()
            ())))

(provide 'rofrol-system)
