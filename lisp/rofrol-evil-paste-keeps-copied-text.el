;; https://github.com/subsid/emacs.d/blob/a3708034aed3f7ea54ac8b41782ca620cbfc44a2/config/evil-bypass-clipboard.el#L34
;; https://web.archive.org/web/20150313145313/http://www.codejury.com/bypassing-the-clipboard-in-emacs-evil-mode/
;; https://www.reddit.com/r/emacs/comments/ct3ijt/solution_to_not_copy_replaced_text_when_pasting/

(defmacro without-evil-mode (&rest do-this)
  ;; Check if evil-mode is on, and disable it temporarily
  `(let ((evil-mode-is-on (evil-mode?)))
     (if evil-mode-is-on
         (disable-evil-mode))
     (ignore-errors
       ,@do-this)
     (if evil-mode-is-on
         (enable-evil-mode))))

(defmacro evil-mode? ()
  "Checks if evil-mode is active. Uses Evil's state to check."
  `evil-state)

(defmacro disable-evil-mode ()
  "Disable evil-mode with visual cues."
  `(progn
     (evil-mode 0)
     (message "Evil mode disabled")))

(defmacro enable-evil-mode ()
  "Enable evil-mode with visual cues."
  `(progn
     (evil-mode 1)
     (message "Evil mode enabled")))

;;;; Clipboard bypass

;; delete: char
(evil-define-operator evil-destroy-char (beg end type register yank-handler)
  :motion evil-forward-char
  (evil-delete-char beg end type ?_))

;; delete: char (backwards)
(evil-define-operator evil-destroy-backward-char (beg end type register yank-handler)
  :motion evil-forward-char
  (evil-delete-backward-char beg end type ?_))

;; delete: text object
(evil-define-operator evil-destroy (beg end type register yank-handler)
  "Vim's 's' without clipboard."
  (evil-delete beg end type ?_ yank-handler))

;; delete: to end of line
(evil-define-operator evil-destroy-line (beg end type register yank-handler)
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (evil-delete-line beg end type ?_ yank-handler))

;; delete: whole line
(evil-define-operator evil-destroy-whole-line (beg end type register yank-handler)
  :motion evil-line
  (interactive "<R><x>")
  (evil-delete-whole-line beg end type ?_ yank-handler))

;; change: text object
(evil-define-operator evil-destroy-change2 (beg end type register yank-handler delete-func)
  (progn
    ;;(message "type: %s" type)
  (evil-change beg
    ;; need this, otherwise cw will delete space after word
    (if (eq type 'exclusive) (- end 1) end)
	        type ?_ yank-handler delete-func)))

(evil-define-operator evil-destroy-change (beg end type register yank-handler delete-func)
  (evil-change beg end type ?_ yank-handler delete-func))

;; https://www.reddit.com/r/emacs/comments/cu0o9j/help_evilchange_removes_one_character_more_than/
(push #'evil-destroy-change evil-change-commands)

;; paste: before
(defun evil-destroy-paste-before ()
  (interactive)
  (without-evil-mode
     (delete-region (point) (mark))
     (evil-paste-before 1)))

;; paste: after
(defun evil-destroy-paste-after ()
  (interactive)
  (without-evil-mode
     (delete-region (point) (mark))
     (evil-paste-after 1)))

;; paste: text object
(evil-define-operator evil-destroy-replace (beg end type register yank-handler)
  (evil-destroy beg end type register yank-handler)
  (evil-paste-before 1 register))

;; Clipboard bypass key rebindings

;; I want this to use kill-ring
;;(define-key evil-normal-state-map "d" 'evil-destroy)
;;(define-key evil-normal-state-map "D" 'evil-destroy-line)
;;(define-key evil-normal-state-map "x" 'evil-destroy-char)
;;(define-key evil-normal-state-map "X" 'evil-destroy-whole-line)

;; When using this I got error: Wrong type argument: commandp
;;(define-key evil-normal-state-map "Y" 'evil-copy-to-end-of-line)

;;(define-key evil-visual-state-map "P" 'evil-destroy-paste-before)
;;(define-key evil-visual-state-map "p" 'evil-destroy-paste-after)
;; evil-destroy-paste-before works as in vim, replaces text without moving
(define-key evil-visual-state-map "p" 'evil-destroy-paste-before)
(define-key evil-normal-state-map "c" 'evil-destroy-change)

(provide 'rofrol-evil-paste-keeps-copied-text)

;; Maybe this could work https://github.com/syl20bnr/spacemacs/issues/5288#issuecomment-254346005

;; does not work in evil when pasting on selection, also needed to add delete-region
;; Paste and replace selection multiple times
;; https://stackoverflow.com/questions/5823495/emacs-how-to-yank-the-last-yanked-text-regardless-of-subsequent-kills/5825012#5825012

;; does not work
;; https://stackoverflow.com/questions/5823495/emacs-how-to-yank-the-last-yanked-text-regardless-of-subsequent-kills/31867563#31867563

;; does not work
;; https://stackoverflow.com/questions/3786895/can-i-keep-the-same-item-for-yanks-in-emacs/3788250#3788250

;; https://stackoverflow.com/questions/3786895/can-i-keep-the-same-item-for-yanks-in-emacs
;; https://stackoverflow.com/questions/5823495/emacs-how-to-yank-the-last-yanked-text-regardless-of-subsequent-kills
;; https://www.reddit.com/r/emacs/comments/30g5wo/the_kill_ring_and_the_clipboard/
;; https://www.reddit.com/r/emacs/comments/2ny06e/delete_text_not_kill_it_into_killring/
;; https://www.reddit.com/r/emacs/comments/58lf3o/in_evil_mode_how_can_i_prevent_adding_to_the_kill/
;; https://emacs.stackexchange.com/questions/12279/evil-is-there-a-default-register-that-does-not-get-overwritten-by-later-yanks-t
