;; Configure `display-buffer' behaviour for some special buffers.
;; https://github.com/lunaryorn/old-emacs-configuration/blob/master/init.el
;; https://www.reddit.com/r/emacs/comments/7au3hj/how_do_you_manage_your_emacs_windows_and_stay_sane/dpgp0fm/
;; https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Displaying-Buffers-in-Side-Windows.html#Displaying-Buffers-in-Side-Windows
;; http://endlessparentheses.com/validate-el-schema-validation-for-emacs-lisp.html
;; Also Emacs Winner-Mode https://youtu.be/T_voB16QxW0
(validate-setq
 display-buffer-alist
 `(
   ;; Put REPLs and error lists into the bottom side window
   (,(rx bos
         (or "*Help"                         ; Help buffers
             "*Warning*"                     ; Emacs warnings
             "*Warnings*"                    ; Emacs warnings
	     "*Backtrace*"                   ; When on *scratch*
             "*Apropos*"                     ; Apropos
             "*Compile-Log*"                 ; Emacs byte compiler log
             "*compilation"                  ; Compilation buffers
             "*Flycheck errors*"             ; Flycheck error list
             "*shell"                        ; Shell window
             "*sbt"                          ; SBT REPL and compilation buffer
             "*ensime-update*"               ; Server update from Ensime
             "*SQL"                          ; SQL REPL
             "*Cargo"                        ; Cargo process buffers
             "*elm-make*"
             "*elm-test*"
	     "*Disabled Command*"
	     "*Annotate"
             (and (1+ nonl) " output*")      ; AUCTeX command output
             ))
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side            . bottom)
    (reusable-frames . visible)
    (window-height   . 0.33))
    (,(rx bos
          (or "*Occur"
              (and (1+ nonl) " output*")      ; AUCTeX command output
              ))
     (display-buffer-reuse-window
      display-buffer-in-side-window)
     (side            . right)
     (reusable-frames . visible)
     (window-height   . 0.33))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; later entry with more specific actions.
   ("." nil (reusable-frames . visible))))

;; https://github.com/yauhen-l/emacs-config/blob/de3d722e844138e6e2a5f8688a3bbb34427430e1/utils/buffer.el#L69
;; better than https://github.com/syl20bnr/spacemacs/issues/1424 because does not close main window
(defun my/quit-bottom-side-windows ()
  "Quit bottom side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list nil 'bottom))
    (when (eq (window-parameter window 'window-side) 'bottom)
      (delete-window window))))

;; conflicts with undo
;;(advice-add 'keyboard-quit :before 'lunaryorn-quit-bottom-side-windows)
(global-set-key (kbd "C-x g") 'my/quit-bottom-side-windows)

(provide 'rofrol-side-windows)
