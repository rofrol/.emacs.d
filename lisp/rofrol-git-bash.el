;; https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
;;(setq shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
;;(setq explicit-shell-file-name shell-file-name)
;;(setq explicit-bash.exe-args '("--login" "-i"))
;;(setq explicit-bash-args '("--login" "-i"))
;;(setq explicit-bash-args '("-i" "-c"))
;;(setq shell-command-switch "-ic")
;;(setq comint-prompt-read-only t)

;; I'm disabling below becuase of problems such as:
;; C-c in elm-mode: /bin/bash: elm-make: command not found
;; /bin/bash: rg: command not found
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;;(setq explicit-shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
;;(setq shell-file-name "bash")
;;(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
;;(setenv "SHELL" shell-file-name)
;;(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)


;; https://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode
;; https://github.com/michalrus/dotfiles/blob/c4421e361400c4184ea90a021254766372a1f301/.emacs.d/init.d/040-terminal.el.symlink#L26-L48
(setq comint-prompt-read-only t)

(defun my-comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-turn-buffer-read-only)

;; so that gnu find is on PATH before windows find
;; https://emacs.stackexchange.com/questions/27326/gui-emacs-sets-the-exec-path-only-from-windows-environment-variable-but-not-from
;; maybe this conflicts with setting bash for shell
;; https://emacs.stackexchange.com/questions/31515/garbage-prepended-to-path
;;(when (eq system-type 'windows-nt)
;;  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
;;  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

;; needed to have sh in eshell
;; https://www.reddit.com/r/emacs/comments/4z8gpe/using_bash_on_windows_for_mx_shell/d6wmc88/
(setenv  "PATH" (concat
     "C:/Program Files/Git/usr/bin" ";"
     (getenv "PATH")
     ))

(provide 'rofrol-git-bash)
