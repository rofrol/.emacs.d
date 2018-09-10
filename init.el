;; -*- lexical-binding: t; -*-

;; https://github.com/lewang/flx
(setq gc-cons-threshold 200000000)


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

(setq load-prefer-newer t)

;; Modularization based on
;; https://github.com/tonini/emacs.d
;; https://www.reddit.com/r/emacs/comments/3q50do/best_way_organization_config_files_in_the_emacs/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'rofrol-system)
(require 'rofrol-display)
(require 'rofrol-utils)

;; https://www.reddit.com/r/emacs/comments/29zm3q/how_to_get_rid_of_filename_files_that_emacs_is/
(setq create-lockfiles nil)
(define-coding-system-alias 'UTF-8 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)
;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
;; https://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs
;; https://emacs.stackexchange.com/questions/22727/pasting-text-from-clipboard-why-m-instead-of-linebreaks
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-16-le)) ;; to paste i.e. ' instead of \222

(use-package validate
  :straight t
  :demand t)

(use-package f
  :straight t
  :demand t)

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
	(defconst elm-package-json
	  "elm-package.json"
	  "The name of the package JSON configuration file.")
 
	(defun rofrol/elm--find-dependency-file-path ()
	  "Recursively search for a directory containing a package JSON file."
	  (or (locate-dominating-file default-directory elm-package-json)
	      (file-name-as-directory (f-dirname (buffer-file-name)))))

	(defun rofrol/elm--has-dependency-file ()
	  "Check if a dependency file exists."
	  (f-exists? (f-join (rofrol/elm--find-dependency-file-path) elm-package-json)))
 
	(defun rofrol/elm-mode-generate-tags ()
	  "Generate a TAGS file for the current project."
	  (interactive)
	  (message "Running rofrol/elm-mode-generate-tags")
	  (when (and (rofrol/elm--has-dependency-file) (eq major-mode 'elm-mode))
	    (let* ((default-directory (rofrol/elm--find-dependency-file-path))
	          (ctags-command "rg --files -telm | ctags -e -L -"))
	          (call-process-shell-command (concat ctags-command "&") nil 0))))
	
	(add-hook 'after-save-hook 'rofrol/elm-mode-generate-tags nil 'make-it-local)
	
	(defun init-elm-mode ()
          "Disable electric-indent-mode and let indentation cycling feature work"
          (if (fboundp 'electric-indent-local-mode)
              (electric-indent-local-mode -1))
          (add-to-list 'company-backends 'company-elm)
          (setq elm-format-on-save t)
          (setq tags-revert-without-query 1)
          (setq elm-tags-on-save t))

	(add-hook 'elm-mode-hook 'init-elm-mode))

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
        (setq projectile-enable-caching t))

      ;; FIXME: too slow while getting submodule files on Windows
      (setq projectile-git-submodule-command ""))
      ;; https://emacs.stackexchange.com/questions/10465/turn-on-projectile-mode-only-for-files-in-actual-projects
      ;; https://gist.github.com/hlissner/f80647f7a390bfe78a805a40b9c28e9b
      ;; https://emacs.stackexchange.com/questions/38759/projectile-buffer-names-with-project-relative-filenames
      (setq projectile-mode-line
	    '(:eval (if (projectile-project-p)
			(format " Projectile[%s]"
				(concat (projectile-project-name) "/"
					(if (eq nil buffer-file-name)
					    ""
					  (f-dirname (f-relative buffer-file-name (projectile-project-root))))))
		      ""))))


;; https://emacs.stackexchange.com/questions/35432/how-to-set-projectile-project-name-as-frame-title
;; https://github.com/syl20bnr/spacemacs/issues/2139
;; Set my-projectile-project-name to projectile-project-name,
;; so that later I can also set projectile project name when in *Messages*
(defun my-projectile-switch-project-action ()
  (set-frame-parameter nil 'my-projectile-project-name projectile-project-name)
  (projectile-run-eshell)
  (projectile-find-file))
  ;; (ivy-switch-buffer))

(setq projectile-switch-project-action 'my-projectile-switch-project-action)

(setq frame-title-format
    '(""
      "%b"
      (:eval
          (if (projectile-project-p)
              (let ((project-name (projectile-project-name)))
                  (if (not (string= "-" project-name))
                    (format " in [%s]" (concat (project-name) "/"
					(if (eq nil buffer-file-name)
					    ""
					  (f-dirname (f-relative buffer-file-name (projectile-project-root))))))
                    (format " in [%s]" (frame-parameter nil 'my-projectile-project-name))))
	      ""))))


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


;; No need as now C-g closes side window
;; auto-switch to help buffer, then press q to close
;; https://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
;; https://stackoverflow.com/questions/1212426/how-do-i-close-an-automatically-opened-window-in-emacs
;; https://stackoverflow.com/questions/11106377/is-there-a-way-to-undo-split-window-below-in-emacs
;;(setq help-window-select t)

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

;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; needed for counsel-find-file to be active etc.
;; http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/OB
(use-package counsel
   :after ivy
   :straight t
   :bind
       (("M-y" . counsel-yank-pop)
       :map ivy-minibuffer-map
           ("M-y" . ivy-next-line)))

;; (use-package counsel-projectile
;;    :after (counsel projectile)
;;    :straight t
;;    :defer t)

;; Technically part of swiper, but we'll configure it here.
(use-package ivy
  :straight t
  :delight
  :init
  (ivy-mode 1)
  :bind
  ("C-s" . swiper)
  ("C-x C-b" . ivy-switch-buffer)
  :config
  ;; show recently opened files when ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; https://emacs.stackexchange.com/questions/31947/distinguish-ido-ivys-virtual-buffers-with-equal-names-using-directory
  (setq ivy-virtual-abbreviate 'full)
  ;; there is also counsel-projectile
  (setq projectile-completion-system 'ivy)
  ;;
  (setq ivy-count-format "%d/%d "))

;; ;; https://github.com/seagle0128/.emacs.d/blob/f8f026da759f32e2d25bab9b2b4c02b73cbbf5ed/lisp/init-ivy.el#L149
;; ;; More friendly display transformer for Ivy
;; (use-package ivy-rich
;;   :straight t
;;   :after (ivy counsel-projectile)
;;   :init
;;   (setq ivy-virtual-abbreviate 'full
;;         ivy-rich-switch-buffer-align-virtual-buffer t)
;;   (setq ivy-rich-path-style 'abbrev)

;;   (ivy-set-display-transformer 'ivy-switch-buffer
;;                                  'ivy-rich-switch-buffer-transformer)
;;   (setq ivy-virtual-abbreviate 'full
;;       ivy-rich-switch-buffer-align-virtual-buffer t)
;;   ;; https://github.com/Yevgnen/ivy-rich/issues/2
;;   (ivy-set-display-transformer
;;    'counsel-projectile-switch-to-buffer 'ivy-rich-switch-buffer-transformer))

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


(use-package crux
  :straight t
  :init (add-hook 'after-init-hook #'projectile-mode)
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

(setq confirm-kill-emacs 'y-or-n-p)


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hyperlinking.html
;; this is better than browse-url-at-mouse, because only hyperlinks can be clicked, not sth like some.text
;; https://emacs.stackexchange.com/questions/30521/rendering-urls-as-clickable-links/30522#30522
;; there is also goto-address-mode https://emacs.stackexchange.com/questions/27094/how-to-make-hyperlinks-clickable-in-markdown-mode/27100#27100
;; which will make hyperlinks clickable
;; which can be activated for certain modes `(add-hook 'mh-show-mode-hook 'goto-address)` or `M-x goto-address`
(global-set-key [C-down-mouse-1] 'ffap-at-mouse)
(define-key global-map (kbd "<C-mouse-1>") 'ignore)

;; Ctrl click a link, also disables mouse-buffer-menu
;; https://www.emacswiki.org/emacs/BrowseUrl
;;(global-set-key [C-down-mouse-1] 'browse-url-at-mouse)


;; https://superuser.com/questions/521223/shift-click-to-extend-marked-region
;; also disables mouse-appearance-menu, but I get S-mouse-1 undefined, so ignoring it
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)
(define-key global-map (kbd "<S-mouse-1>") 'ignore)

;; this appears not needed
;; https://stackoverflow.com/questions/11176853/understanding-emacs-cua-mode-for-shift-click-selection
;; shift + click select region
;(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
;(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
;(put 'mouse-set-point 'CUA 'move)

;; https://stackoverflow.com/questions/13986605/how-to-make-emacs-mouse-drag-not-highlight-or-set-mark

;; https://emacs.stackexchange.com/questions/7244/enable-emacs-column-selection-using-mouse
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "C-S-<down-mouse-1>") #'mouse-start-rectangle)

;; show border around zero width space ​ https://news.ycombinator.com/item?id=16754256
;; to insert: `C-x 8 RET` then search for ZERO WIDTH SPACE
;; or `C-q 20013 RET'
(update-glyphless-char-display 'glyphless-char-display-control '((format-control . empty-box) (no-font . hex-code)))

;; https://jblevins.org/projects/markdown-mode/
;; either `C-c C-x C-l` or `M-x markdown-toggle-url-hiding` or add `(markdown-toggle-url-hiding t)` to your markdown-mode-hook
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
      (add-hook 'markdown-mode-hook 'markdown-toggle-url-hiding))

(use-package goto-addr
  :init
  (setq goto-address-mail-face 'link)
  :config
  ;; http://www.bartuka.com/emacs/2018/02/02/bartuka's-emacs-config.html
  (add-hook 'prog-mode-hook 'goto-address-prog-mode))

;; https://www.emacswiki.org/emacs/KillBufferUnconditionally
;; https://stackoverflow.com/questions/6467002/how-to-kill-buffer-in-emacs-without-answering-confirmation
;; https://superuser.com/questions/632750/how-can-i-eliminate-prompts-and-dialog-boxes-in-emacs-and-enable-auto-saving
;; https://emacs.stackexchange.com/questions/3245/kill-buffer-prompt-with-option-to-diff-the-changes
;; https://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; looks like below is not needed, but in the link there is example to expand it to *scratch* etc.
;; http://amitp.blogspot.com/2007/03/emacs-dont-kill-unsaved-buffers.html
;;(defun ask-before-killing-buffer ()
;; (cond
;;  ((and buffer-file-name (buffer-modified-p))
;;   (y-or-n-p (format "Buffer %s modified; kill anyway? "
;;                 (buffer-name))))
;;  (t t)))
;;(add-to-list 'kill-buffer-query-functions 'ask-before-killing-buffer)

;; .dir-locals.el are save to init file as custom,
;; so let's specify custom file and gitignore it
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

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


;; https://unix.stackexchange.com/questions/9740/is-there-a-convenient-general-way-to-grab-the-echoed-result-of-a-command-in-em/24287#24287
;; or just `C-x h` then `M-w`
;; https://emacs.stackexchange.com/questions/37180/how-copy-content-of-minibuffer-to-kill-ring/37181#37181
(defun c5-eval-to-kill-ring ()
  (interactive)
  (kill-new (with-output-to-string (princ (call-interactively 'eval-expression)))))

(global-set-key (kbd "C-;") 'comment-line)


;; https://stackoverflow.com/questions/1072662/by-emacs-how-to-join-two-lines-into-one/17682863#17682863
(defun join-lines (arg)
  (interactive "p")
  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " "))

(global-set-key (kbd "M-^") 'join-lines)

;; https://gist.github.com/meeiw/3082383
(defun yank-projectile-relative-path-with-line-number ()
  "Yank current projectile relative path and line number as '<path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (f-relative buffer-file-name (projectile-project-root)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

;; `C-x w .` to highlight-symbol-at-point
;; `C-u C-x w r' to unhighligh all
;; `C-x w h` to hightligh-regexp
;; `C-u C-x w r' to unhighlight-regexp
;; https://stackoverflow.com/questions/385661/how-to-highlight-all-occurrences-of-a-word-in-an-emacs-buffer
;; https://emacs.stackexchange.com/questions/19861/how-to-unhighlight-symbol-highlighted-with-highlight-symbol-at-point
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Highlight-Interactively.html
(global-hi-lock-mode 1)

;; https://www.reddit.com/r/emacs/comments/5g508b/elisp_binding_digitargument_in_a_sparsetransient/
(setq diego/vert-window-transient-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "=") #'diego/vert-enlarge-window)
        (define-key map (kbd "-") #'diego/vert-shrink-window)
        map))

(defun diego/vert-enlarge-window (arg)
  (interactive "P")
  (if (eq arg nil)
      (enlarge-window 1)
    (enlarge-window arg))
  (set-transient-map
   diego/vert-window-transient-map))

(defun diego/vert-shrink-window (arg)
  (interactive "P")
  (if (eq arg nil)
      (shrink-window 1)
    (shrink-window arg))
  (set-transient-map
   diego/vert-window-transient-map))

;; find-file with line number like src/ChartBuilder.elm:1420
;; use with projectile-find-file
;; https://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax/46273760#46273760
(defun find-file--line-number (orig-fun filename &optional wildcards)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      (apply orig-fun (list filename wildcards))
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

(advice-add 'find-file :around #'find-file--line-number)


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

;; https://stackoverflow.com/questions/10018815/registering-click-events-for-emacs-lisp
;; (define-key global-map (kbd "<down-mouse-1>")
;;   (lambda (event)
;;     (interactive "e")
;;     (message "%s" event)
;;     (let ((posn (elt event 1)))
;;       (with-selected-window (posn-window posn)
;;         (goto-char (+ (posn-point posn) -1))
;;         (redisplay)))))


;; https://emacs.stackexchange.com/questions/392/how-to-change-the-cursor-type-and-color
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Cursor-Display.html
;; (setq-default cursor-type 'bar)
(setq-default cursor-type '(bar . 2))
;; (setq-default cursor-type 'hollow)


;; newline-without-break-of-line
;; https://stackoverflow.com/questions/5898448/how-to-add-a-new-line-without-breaking-the-current-line/41540936#41540936
;; Insert new line below current line
;; and move cursor to new line
;; it will also indent newline
(global-set-key (kbd "<C-return>") (lambda ()
                   (interactive)
                   (end-of-line)
                   (newline-and-indent)))
;; Insert new line above current line
;; and move cursor to previous line (newly inserted line)
;; it will also indent newline
;; TODO: right now I am unable to goto previous line, FIXIT
(global-set-key (kbd "<C-S-return>") (lambda ()
                       (interactive)
                       (beginning-of-line)
                       (newline-and-indent)
                       (previous-line)))

;; https://stackoverflow.com/questions/2249955/emacs-shift-tab-to-left-shift-the-block/35183657#35183657
;; https://stackoverflow.com/questions/11623189/how-to-bind-keys-to-indent-unindent-region-in-emacs
(defun rofrol/indent-region(numSpaces)
    (progn 
        ; default to start and end of current line
        (setq regionStart (line-beginning-position))
        (setq regionEnd (line-end-position))

        ; if there's a selection, use that instead of the current line
        (when (use-region-p)
            (setq regionStart (region-beginning))
            (setq regionEnd (region-end))
        )

        (save-excursion ; restore the position afterwards            
            (goto-char regionStart) ; go to the start of region
            (setq start (line-beginning-position)) ; save the start of the line
            (goto-char regionEnd) ; go to the end of region
            (setq end (line-end-position)) ; save the end of the line

            (indent-rigidly start end numSpaces) ; indent between start and end
            (setq deactivate-mark nil) ; restore the selected region
        )
    )
)

(defun rofrol/indent-lines(&optional N)
    (interactive "p")
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    (* (or N 1) tab-width)))

(defun rofrol/untab-region (&optional N)
    (interactive "p")
    (rofrol/indent-region (* (* (or N 1) tab-width)-1)))

(defun  rofrol/tab-region (N)
    (interactive "p")
    (if (use-region-p)
        (rofrol/indent-region (* (or N 1) tab-width)) ; region was selected, call indent-region
        (rofrol/indent-lines N); else insert spaces as expected
    ))

(global-set-key (kbd "C->") 'rofrol/tab-region)
(global-set-key (kbd "C-<") 'rofrol/untab-region)



;; https://www.reddit.com/r/emacs/comments/8us8wi/make_inserting_cursor_easier_for_kids/
(use-package "mouse+"
  :straight t
  :config
    ; Highlight yank position or call `M-x' in echo area.
    (global-set-key [down-mouse-2]   'mouse-flash-position-or-M-x) ; `mouse-2'
    ;; Highlight line or `M-:'.
    (global-set-key [S-down-mouse-2] 'mouse-scan-lines-or-M-:) ; `S-mouse-2'
    (global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)
    (define-key minibuffer-inactive-mode-map [down-mouse-1] nil) ; `mouse-1'
    (define-key minibuffer-inactive-mode-map [mouse-1] nil))

 (defvar mouse-position-previous-handler
   (lookup-key special-event-map [mouse-movement]))
 
 (defun mouse-position (ev)
   (interactive "e")
   (goto-char (posn-point (event-start ev))))
 
 (defun enable-mouse-position ()
   (interactive)
   (setq mouse-position-previous-handler
         (lookup-key special-event-map [mouse-movement]))
   (setq track-mouse t)
   (define-key special-event-map [mouse-movement]
     'mouse-position))

 (defun disable-mouse-position ()
   (interactive)
   (setq track-mouse nil)
   (define-key special-event-map [mouse-movement]
     mouse-position-previous-handler)
   (setq mouse-position-previous-handler nil))


;; remove git branch from modeline
;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line
(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git.*$" " " vc-mode))))

;; C-mouse1
;; https://www.reddit.com/r/emacs/comments/22tync/is_it_possible_to_use_findfileatpoint_with/
(push (cons 'text-mode
            (lambda (name)
              (let ((newname (expand-file-name name (projectile-project-root))))
                (when (file-exists-p newname)
                  newname))))
      ffap-alist)

;; https://gist.github.com/sky-y/3263051
;; Open the file name being pointed in an other window or dired
;; reference: http://kouzuka.blogspot.com/2011/02/emacsurlfinder.html
(defun my-directory-or-file-p (path)
  "return t if path is a directory,
return nil if path is a file"
  (car (file-attributes path)))

(defun my-open-emacs-at-point ()
  "open the file with opening emacs"
  (interactive)
  (require 'ffap)
  (let ((file (or (ffap-url-at-point)
                  (ffap-file-at-point))))
    (unless (stringp file)
      (error"No file or URL found"))
    (when (file-exists-p (expand-file-name file))
      (setq file (expand-file-name file)))
    (message "Open: %s" file)

    (if (my-directory-or-file-p file)
      (dired-other-window file)
      (find-file-other-window file))
    
    ))

(global-set-key (kbd "\C-c o") 'my-open-emacs-at-point)
(global-set-key (kbd "C-M-<mouse-1>") 'my-open-emacs-at-point)

;; (global-set-key (kbd "C-c s") 'counsel-projectile-rg)
(global-set-key (kbd "C-c s") 'counsel-rg)


;;(setq-default mouse-wheel-flip-direction t)

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

(use-package typescript-mode
  :straight t)

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

(defun elm-occur ()
 "Elm and occure searching for lines with definitions and annotations"
 (interactive)
 ;; (occur "^[a-z].*\\(:.+$\\|=$\\)"))
 ;; (occur "^[a-z].*=$"))
 (occur "^\\([a-z].*=$\\|type \\|port .*:\\|--\\)"))

;; https://stackoverflow.com/questions/586735/how-can-i-check-if-a-current-buffer-exists-in-emacs/2050989#2050989
(defun buffer-exists (bufname)   (not (eq nil (get-buffer bufname))))
(defun elm-occur-toggle ()
  (interactive)
  (let ((buffers (seq-filter (lambda (window)
		       (string-prefix-p "*Occur: " (buffer-name (window-buffer window))))
		  (window-list))))
    (if (not (eq 0 (length buffers)))
	(kill-buffer (window-buffer (car buffers)))
      (elm-occur)
      (occur-rename-buffer))))

(global-set-key [f5] 'elm-occur-toggle)

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


;; require library cl at compile time, to get the use of its macros (and not get any runtime load). That is where macro lexical-let is defined. https://emacs.stackexchange.com/questions/15189/alternative-to-lexical-let/15191#15191
;; (eval-when-compile (require 'cl))

;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
(defun bury-compile-buffer-if-successful (name warning)
  (lambda(buf desc)
    (if (and (null (string-match ".*exited abnormally.*" desc))
	     (not (with-current-buffer buf (search-forward warning nil t))))
	;;no errors, make the compilation window go away in a few seconds
	(progn
	  (run-at-time "2 sec" nil 'delete-windows-on (get-buffer-create name))
	  (message "No Compilation Errors!")))))

;; https://emacs.stackexchange.com/questions/32544/function-that-takes-a-function-as-argument-and-returns-a-new-function#comment50322_32546
(fset 'bury-compile-buffer-if-successful-elm
      (bury-compile-buffer-if-successful "*elm-make*" "WARNING"))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful-elm)

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
