(use-package f
  :straight t
  :demand t)


;; not sure if I need this, when counsel-rg shows results live
;; and to have list in buffer and I can press `C-c C-o` which is `ivy-occur`
;; for projectile-ripgrep
;;(use-package ripgrep
;;    :straight t
;;    :defer t)

;; needed for counsel-find-file to be active etc.
;; http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/OB
(use-package counsel
   :after ivy
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


;; https://unix.stackexchange.com/questions/9740/is-there-a-convenient-general-way-to-grab-the-echoed-result-of-a-command-in-em/24287#24287
;; or just `C-x h` then `M-w`
;; https://emacs.stackexchange.com/questions/37180/how-copy-content-of-minibuffer-to-kill-ring/37181#37181
(defun c5-eval-to-kill-ring ()
  (interactive)
  (kill-new (with-output-to-string (princ (call-interactively 'eval-expression)))))

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


;; https://stackoverflow.com/questions/10018815/registering-click-events-for-emacs-lisp
;; (define-key global-map (kbd "<down-mouse-1>")
;;   (lambda (event)
;;     (interactive "e")
;;     (message "%s" event)
;;     (let ((posn (elt event 1)))
;;       (with-selected-window (posn-window posn)
;;         (goto-char (+ (posn-point posn) -1))
;;         (redisplay)))))



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

;; https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the 
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))

(add-to-list 'Info-default-directory-list "/usr/share/info/")

;; https://github.com/whatyouhide/emacs.d/blob/master/init.el
;; (use-package sublime-themes
;;   :straight t
;;   :init (progn
;; 	  (load-theme 'ritchie t)))

(provide 'rofrol-rest)
