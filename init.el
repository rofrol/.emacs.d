(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;
(setq load-prefer-newer t)

;; Modularization based on
;; https://github.com/tonini/emacs.d
;; https://www.reddit.com/r/emacs/comments/3q50do/best_way_organization_config_files_in_the_emacs/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'rofrol-system)
(require 'rofrol-display)
(require 'rofrol-utils)

(use-package company
  :straight t
  :config
  (global-company-mode))

;; required for company-elm
;;(add-hook 'after-init-hook 'global-company-mode)

(use-package elm-mode
    :mode ("\\.elm\\'" . elm-mode)
    :straight t
    :init
    (progn
      (defun init-elm-mode ()
        "Disable electric-indent-mode and let indentation cycling feature work"
        (if (fboundp 'electric-indent-local-mode)
            (electric-indent-local-mode -1))
        (add-to-list 'company-backends 'company-elm)
        (setq elm-format-on-save t))

      (add-hook 'elm-mode-hook 'init-elm-mode)))

;; not sure if I need this, when counsel-rg shows results live
;; and to have list in buffer and I can press `C-c C-o` which is `ivy-occur`
;; for projectile-ripgrep
;;(use-package ripgrep
;;    :straight t
;;    :defer t)

;; ripgrep
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-projectile.el
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-projectile.el
;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile/29200#29200
(use-package projectile
  :straight t
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
    ;; use rg for find-file
    (let ((command
           (cond
            ((executable-find "rg")
             (let ((rg-cmd ""))
               (dolist (dir projectile-globally-ignored-directories)
                 (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
               (concat "rg -0 --files --color=never --hidden" rg-cmd))))))
    (setq projectile-generic-command command))

    ;; Faster searching on Windows
    (when (eq system-type 'windows-nt)
      (when (or (executable-find "rg") (executable-find "pt") (executable-find "ag"))
        (setq projectile-indexing-method 'alien)
        (setq projectile-enable-caching nil))

      ;; FIXME: too slow while getting submodule files on Windows
      (setq projectile-git-submodule-command ""))
      ;; commenting out, because I have it in frame title atm
      ;; https://emacs.stackexchange.com/questions/10465/turn-on-projectile-mode-only-for-files-in-actual-projects
      ;;(setq projectile-mode-line
      ;;'(:eval (if (projectile-project-p)
      ;;            (format " Projectile[%s]"
      ;;                    (projectile-project-name))
      ;;          "")))
   )


;; https://emacs.stackexchange.com/questions/35432/how-to-set-projectile-project-name-as-frame-title
;; https://github.com/syl20bnr/spacemacs/issues/2139
;; Set my-projectile-project-name to projectile-project-name,
;; so that later I can also set projectile project name when in *Messages*
(defun my-projectile-switch-project-action ()
  (set-frame-parameter nil 'my-projectile-project-name projectile-project-name)
  (projectile-run-eshell)
  (projectile-find-file))

(setq projectile-switch-project-action 'my-projectile-switch-project-action)

(setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
           (if (not (string= "-" project-name))
             (format " in [%s]" project-name)
             (format " in [%s]" (frame-parameter nil 'my-projectile-project-name)))))))


;; https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

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


;; auto-switch to help buffer, then press q to close
;; https://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
;; https://stackoverflow.com/questions/1212426/how-do-i-close-an-automatically-opened-window-in-emacs
;; https://stackoverflow.com/questions/11106377/is-there-a-way-to-undo-split-window-below-in-emacs
(setq help-window-select t)

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

;; needed for counsel-find-file to be active etc.
;; http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/OB
(use-package counsel
   :straight t
   :bind
       (("M-y" . counsel-yank-pop)
       :map ivy-minibuffer-map
           ("M-y" . ivy-next-line)))

;; Technically part of swiper, but we'll configure it here.
(use-package ivy
  :straight t
  :delight
  :init
      (ivy-mode 1)
  :config
      ;; show recently opened files when ivy-switch-buffer
      (setq ivy-use-virtual-buffers t)
      ;; https://emacs.stackexchange.com/questions/31947/distinguish-ido-ivys-virtual-buffers-with-equal-names-using-directory
      (setq ivy-virtual-abbreviate 'full)
      ;; there is also counsel-projectile
      (setq projectile-completion-system 'ivy)
  )

;; On Windows, set HOME to USERPROFILE and create shortcut whith "Start in" set to `%HOME`.
;; https://stackoverflow.com/questions/60464/changing-the-default-folder-in-emacs/60482#60482
;; http://ergoemacs.org/emacs/emacs_mswin.html
;; set for every find-file https://stackoverflow.com/questions/6464003/emacs-find-file-default-path/6465677#6465677
;; inhibiting and setting below didn't work.
;;(cd (getenv "HOME")) ;; doesn't work
;;(setq default-directory "~/") ;; doesn't work
;;(add-hook 'find-file-hook #'(lambda () (setq default-directory "~/"))) ;; doesn't work

;; do I need this?
;;(setq-default buffer-file-coding-system 'utf-8-unix)

;; http://iqbalansari.me/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
;;   - https://github.com/magnars/.emacs.d/blob/master/settings/sane-defaults.el#L135
;; https://emacs.stackexchange.com/questions/9951/how-to-set-the-coding-system-for-new-files-with-specific-extensions/9953
;; nice solutions https://superuser.com/questions/131538/can-i-create-directories-that-dont-exist-while-creating-a-new-file-in-emacs
;; touch https://stackoverflow.com/questions/8989540/touch-current-file-in-emacs
;; no good answer here https://emacs.stackexchange.com/questions/425/opening-a-new-file-whose-parent-directory-doesnt-exist-yet
;; alternative: create dirs before save https://stackoverflow.com/questions/6830671/how-to-make-emacs-create-intermediate-dirs-when-saving-a-file/6830894#6830894
;; with dired https://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs/18885461#18885461
;; another https://www.reddit.com/r/emacs/comments/4azsbg/easy_way_to_create_an_empty_new_file_in_emacs/d15w5eh/
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))
          ;; wait until directory is created
          (while (not (file-directory-p parent-directory))
              (sleep-for 1)))
          ;; https://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs/11990694#11990694
          (shell-command (concat "touch " (shell-quote-argument (buffer-file-name)))))
          ;;(set-buffer-modified-p t)) ;; maybe will need it with save after open touch dos not work

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

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

;; also http://ergoemacs.org/emacs/rename_file_pattern.html
(global-set-key (kbd "C-c r") 'crux-rename-file-and-buffer)

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

;; https://emacs.stackexchange.com/questions/7126/run-command-in-new-frame
;; https://emacs.stackexchange.com/questions/42049/open-new-frame-when-switching-between-projects-in-projectile
;; https://github.com/bbatsov/projectile/issues/490
;;(defun run-command-in-new-frame (prefixarg command-name)
;;  (interactive (list current-prefix-arg (read-extended-command)))
;;  (let ((command (intern-soft command-name)))
;;    (unless command
;;      (error "%s is not a valid command name" command-name))
;;    (select-frame (make-frame))
;;    (let ((prefix-arg prefixarg))
;;      (command-execute command))))
;;
;;(defun projectile-switch-project--new-frame (orig-fun &rest args)
;;  (select-frame (make-frame))
;;  (print args)
;;  (apply orig-fun args))
;;
;;;;(advice-add 'projectile-switch-project-by-name :before #'projectile-switch-project--new-frame )
;;
;;(defun amd/projectile-switch-project (old-function &rest arguments)
;;  (message "old-function %s" old-function)
;;  (message ">>>>>>\narguments %s" arguments)
;;  (select-frame (make-frame))
;;  (apply old-function arguments))
;;  
;;(advice-add 'projectile-switch-project-by-name :around #'amd/projectile-switch-project)
