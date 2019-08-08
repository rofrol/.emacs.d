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

;; not needed with evil
;;(setq confirm-kill-emacs 'y-or-n-p)

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
  ;; https://unix.stackexchange.com/questions/7280/in-gnu-emacs-how-do-i-set-up-a-global-key-to-toggle-the-menu-bar/7291#7291
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
        (comment-or-uncomment-region beg end)))
(defun comment-or-uncomment-region-or-line-without_deselect ()
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

;; ----------------------------------------------------------
;; tmux
;; ----------------------------------------------------------
;; TODO: Use perfromant version:
;; Well, the point of this question was how to use meta/control/shift-arrow for these commands, rather than redefine them to something else. I'll also add that I have a more involved version of this linked tmux pane <--> emacs window movement. The problem I find is (1) if something slow is happening in emacs, then while I can move to another tmux window, emacs is tied up processing so I remain frozen in the emacs pane. (2) It doesn't work if I sudo emacs. I'll add (3) that I do get confused if I'm in emacs or tmux and swap the commands by mistake. https://stackoverflow.com/questions/34084245/tmux-interfering-with-emacs-commands-windmove-default-keybindings/56445138?noredirect=1#comment99488129_56445138
;; https://blog.kdheepak.com/emacsclient-and-tmux-split-navigation.html
;; similar but could not install https://github.com/keith/evil-tmux-navigator
;; Try to move direction, which is supplied as arg
;; If cannot move that direction, send a tmux command to do appropriate move
(defun windmove-emacs-or-tmux(dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                       ;; Moving within emacs
    (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

;; Move between windows with custom keybindings
;; Use with ~/bin/tmux_navigate.sh in ~/.tmux.conf
(global-set-key (kbd "<S-up>")
		'(lambda () (interactive) (windmove-emacs-or-tmux "up"  "if [ $(tmux display-message -p '#{pane_at_top}') -ne 1 ]; then tmux select-pane -U; fi")))
(global-set-key (kbd "<S-down>")
		'(lambda () (interactive) (windmove-emacs-or-tmux "down"  "if [ $(tmux display-message -p '#{pane_at_bottom}') -ne 1 ] ; then tmux select-pane -D; fi")))
(global-set-key (kbd "<S-right>")
		'(lambda () (interactive) (windmove-emacs-or-tmux "right" "if [ $(tmux display-message -p '#{pane_at_right}') -ne 1 ]; then tmux select-pane -R; fi")))
(global-set-key (kbd "<S-left>")
		'(lambda () (interactive) (windmove-emacs-or-tmux "left"  "if [ $(tmux display-message -p '#{pane_at_left}') -ne 1 ]; then tmux select-pane -L; fi")))

;; copy to system clipboard, paste from system clipboard when emacs run in nowindow mode
;; Based on
;; - https://stackoverflow.com/questions/21830813/how-to-kill-yank-code-between-emacs-buffers-using-screen/21833639#21833639
;; >The clipboard itself is a feature of X11, so you will not be able to use it without having any X11 server instance running anywhere. More about that below.
;; >
;; >However, xclip/xsel don't have the X11 server as dependency, they only need some client libraries installed. You should install xsel though if you want to have as few packages as possible, as it has significantly less dependencies than xclip (compare the output of apt depends --recurse --important xsel and apt depends --recurse --important xclip).
;; >- https://askubuntu.com/questions/1111646/alternative-to-xsel-or-xclip-without-x11-installed
;; - https://stackoverflow.com/questions/4580835/emacs-copy-kill-ring-to-system-clipboard-in-nowindow-mode
;; - https://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux/19625063#19625063
;; - https://stackoverflow.com/questions/4580835/emacs-copy-kill-ring-to-system-clipboard-in-nowindow-mode/4581008
;; - toggle clipboard https://emacs.stackexchange.com/questions/10900/copy-text-from-emacs-to-os-x-clipboard/33315#33315
;; - https://www.emacswiki.org/emacs/Comments_on_CopyAndPaste
;; - https://gist.github.com/pkkm/5522129
;; - https://emacs.stackexchange.com/questions/10900/copy-text-from-emacs-to-os-x-clipboard/10963
;; - https://groups.google.com/forum/#!topic/gnu.emacs.help/38WrcwhH81I
;; - maybe slow on ssh https://stackoverflow.com/questions/27764059/emacs-terminal-mode-how-to-copy-and-paste-efficiently
;; - alternative - allowWindowOps https://emacs.stackexchange.com/questions/22271/clipboard-manager-will-not-work-in-terminal-emacs
;; - more sophisticated detecting if emacs in terminal https://emacs.stackexchange.com/questions/412/copy-and-paste-between-emacs-in-an-x-terminal-and-other-x-applications
;; - check OS https://stackoverflow.com/questions/3216081/integrate-emacs-copy-paste-with-system-copy-paste
;; - check OS https://emacs.stackexchange.com/questions/766/add-operating-system-clipboard-to-kill-ring
;; - another alternative https://stackoverflow.com/questions/18387742/copy-paste-from-emacs-in-command-line/18387899#18387899
;; - https://blog.d46.us/zsh-tmux-emacs-copy-paste/
;; - https://www.reddit.com/r/emacs/comments/9qvssh/copy_text_from_emacs_to_other_programs/
;; - https://github.com/rolandwalker/simpleclip
;; - window-system - The value is nil if the selected frame is on a text-only-terminal.
;; - unless - https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html
(unless window-system
      (when (getenv "DISPLAY")
        ;; Callback for when user cuts
        (defun xsel-cut-function (text &optional push)
          ;; Insert text to temp-buffer, and "send" content to xsel stdin
          (with-temp-buffer
            (insert text)
            ;; I prefer using the "clipboard" selection (the one the
            ;; typically is used by c-c/c-v) before the primary selection
            ;; (that uses mouse-select/middle-button-click)
            (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
        ;; Call back for when user pastes
        (defun xsel-paste-function()
          ;; Find out what is current selection by xsel. If it is different
          ;; from the top of the kill-ring (car kill-ring), then return
          ;; it. Else, nil is returned, so whatever is in the top of the
          ;; kill-ring will be used.
          (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
            (unless (string= (car kill-ring) xsel-output)
              xsel-output )))
        ;; Attach callbacks to hooks
        (setq interprogram-cut-function 'xsel-cut-function)
        (setq interprogram-paste-function 'xsel-paste-function)))

;; https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/czoqvgu/
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(global-set-key (kbd "<f8>") 'xah-pop-local-mark-ring)

;; disabling bc of https://github.com/technomancy/find-file-in-project
;; https://stackoverflow.com/questions/354490/preventing-automatic-change-of-default-directory/354654#354654
;;(add-hook 'find-file-hook
;;          (lambda ()
;;            (setq default-directory command-line-default-directory)))
;;	    ;; does not work
;;            ;;(setq default-directory (vc-root-dir))))

(use-package find-file-in-project
  :straight t
  :config
  (setq ffip-use-rust-fd t)
  (setq ffip-match-path-instead-of-filename t)
  (setq ffip-split-window-without-asking-for-keyword t))

(use-package zoom
  :straight t
  :init
  (zoom-mode t)
  :config
  (defun size-callback ()
    ;; https://stackoverflow.com/questions/5321097/what-does-the-dot-in-the-following-emacs-command-mean/5321201#5321201
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Dotted-Pair-Notation.html
    ;; it is a dotted pair notation.
    ;; first number is for width, second for height
    ;; (0.618 . 0.618) is different than (0.618 0.618). In the second example, height is not balanced.
    ;;(cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
    ;;	  (t                            '(0.618 . 0.618))))
    (cond ((> (frame-pixel-width) 1280) '(0.618 . 0.618))
	  (t                            '(0.8 . 0.8))))
  (setq zoom-size 'size-callback))

;; https://stackoverflow.com/questions/20167246/emacs-open-buffer-in-vertical-split-by-default/20167940#20167940
(split-window-right)

(provide 'rofrol-system)
