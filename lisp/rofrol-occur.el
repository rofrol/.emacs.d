;; https://emacs.stackexchange.com/questions/13212/how-to-make-occur-mode-select-the-window-of-buffer-occur
(add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*")
	     (occur-mode-clean-buffer)))
;; disabling because very slow scrolling
;; https://emacs.stackexchange.com/questions/7281/how-to-modify-face-for-a-specific-buffer
;; https://stackoverflow.com/questions/25109011/how-to-speed-up-a-custom-mode-line-face-change-function-in-emacs
;; (face-remap-add-relative 'match '(:background nil))))
;; maybe more correct but still slow
;; https://emacs.stackexchange.com/questions/35349/what-is-the-correct-way-to-unset-foreground-color
;; (face-remap-add-relative 'match '(:background (face-background 'default)))))
;; https://stackoverflow.com/questions/15733873/customizing-highlighting-faces-in-emacs-only-change-the-background-color
;; (set-face-attribute 'highlight nil :foreground 'unspecified)

;; not needed
;; (next-error-follow-minor-mode)))

;; https://emacs.stackexchange.com/questions/3630/can-occur-center-the-found-text-in-the-buffer
;; (defun foo ()
;;   (let ((line   (line-number-at-pos)))
;;     (cond ((<= line (+ (line-number-at-pos (window-start)) 10))
;;            (recenter 10))
;;           ((>= line (- (line-number-at-pos (window-end)) 10))
;;            (recenter -10)))))

(add-hook 'occur-mode-find-occurrence-hook 'recenter)

;; https://stackoverflow.com/questions/586735/how-can-i-check-if-a-current-buffer-exists-in-emacs/2050989#2050989
(defun buffer-exists (bufname)   (not (eq nil (get-buffer bufname))))

;; https://www.emacswiki.org/emacs/OccurMode
(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (save-excursion
        (set-buffer (get-buffer "*Occur*"))
        (goto-char (point-min))
        (toggle-read-only 0)
        (if (looking-at "^[0-9]+ lines matching \"")
            (kill-line 1))
        (while (re-search-forward "^[ \t]*[0-9]+:"
                                  (point-max)
                                  t)
          (replace-match "")
          (forward-line 1)))
    (message "There is no buffer named \"*Occur*\".")))

(provide 'rofrol-occur)
