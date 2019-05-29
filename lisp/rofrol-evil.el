;; -*- lexical-binding: t; -*-

;; https://www.reddit.com/r/emacs/comments/726p7i/evil_mode_and_use_package/dnh3338/
;; https://github.com/raxod502/straight.el/issues/250
;; https://www.linode.com/docs/tools-reference/tools/emacs-evil-mode/
;; https://github.com/noctuid/evil-guide
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://www.emacswiki.org/emacs/Evil
;; https://www.reddit.com/r/emacs/comments/2mcj85/switching_from_vim_should_i_use_emacs_evil_or/
;; https://github.com/noctuid/evil-guide
;; https://github.com/sunesimonsen/evil-config
;; https://stackoverflow.com/questions/8483182/evil-mode-best-practice
;; C-z -> evil-normal-mode
;; Evil bindings for the parts of Emacs that Evil does not cover properly by default, such as help-mode, M-x calendar, Eshell and more https://github.com/emacs-evil/evil-collection
(use-package evil
  :straight t
  :init ;; tweak evil's configuration before loading it
  ;; (setq evil-search-module 'evil-search)
  ;; (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  ;; (setq evil-shift-round nil)
  ;; (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  ;; Do not replace kill ring with replaced text
  ;; does not work
  ;; - https://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  ;; - https://www.emacswiki.org/emacs/CopyAndPaste
  ;;(fset 'evil-visual-update-x-selection 'ignore)
  ;; does not work
  ;; - https://emacs.stackexchange.com/questions/28135/in-evil-mode-how-can-i-prevent-adding-to-the-kill-ring-when-i-yank-text-visual/46485#46485
  ;;(setq-default evil-kill-on-visual-paste nil)
  :config ;; tweak evil after loading it
  ;; Deleting buffer without losing the split window
  ;; https://implementations-list.ourproject.narkive.com/7gdihu4I/deleting-buffer-without-losing-the-split-window
  ;; same as to https://github.com/qpkorr/vim-bufkill
  (evil-ex-define-cmd "BD[elete]" 'kill-this-buffer)
  ;; need to press enter before typing to live search https://github.com/technomancy/find-file-in-project/issues/116
  (evil-ex-define-cmd "vs" 'ffip-split-window-horizontally)
  (define-key evil-normal-state-map "\gc" 'comment-or-uncomment-region-or-line)
  (evil-mode))

;; https://github.com/syl20bnr/spacemacs/issues/9330#issuecomment-319448299
;; http://wikemacs.org/wiki/Evil
(use-package evil-escape
 :straight t
 :after evil
 :commands evil-escape-mode
 :init
 (evil-escape-mode)
 :config
   (define-key evil-normal-state-map "\C-y" 'yank)
   (define-key evil-insert-state-map "\C-y" 'yank)
   (define-key evil-visual-state-map "\C-y" 'yank)
   (define-key evil-insert-state-map "\C-e" 'end-of-line)
   (define-key evil-normal-state-map "\C-w" 'evil-delete)
   (define-key evil-insert-state-map "\C-w" 'evil-delete)
   (define-key evil-insert-state-map "\C-r" 'search-backward)
   (define-key evil-visual-state-map "\C-w" 'evil-delete)

   ;; When entering commands into the M-x minibuffer, to use Escape to cancel and get back to the main window.
   ;; https://jen20.com/2015/02/06/configuring-emacs-for-go-part-1.html
   ;; https://stackoverflow.com/questions/8483182/evil-mode-best-practice/10166400#10166400
   (define-key evil-normal-state-map [escape] 'keyboard-quit)
   (define-key evil-visual-state-map [escape] 'keyboard-quit)
   (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
   (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
   (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
   (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
   (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
   (global-set-key [escape] 'evil-exit-emacs-state))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))



(provide 'rofrol-evil)
