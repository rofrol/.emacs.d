(set-frame-font "Source Code Pro-13" nil t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

;;; http://ergoemacs.org/emacs/emacs_line_number_mode.html
(when (version<= "26.0.50" emacs-version )
  ;;; enable in all programming modes
  (add-hook 'prog-mode-hook 'global-display-line-numbers-mode))

(column-number-mode t)

(provide 'rofrol-display)
