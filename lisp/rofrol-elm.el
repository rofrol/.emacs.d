;; -*- lexical-binding: t; -*-

;; required for company-elm
(add-hook 'after-init-hook 'global-company-mode)

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

;; depends on rofrol-occur
(defun elm-occur ()
 "Elm and occure searching for lines with definitions and annotations"
 (interactive)
 ;; (occur "^[a-z].*\\(:.+$\\|=$\\)"))
 ;; (occur "^[a-z].*=$"))
 (occur "^\\([a-z].*=$\\|type \\| +[=|] [a-zA-Z ().]*$\\|port .*:\\| *-- BOOKMARK\\|-- \\|.*Debug\\.log\\)"))

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

;; for elm-0.19
(setq elm-package-json "elm.json")

(provide 'rofrol-elm)
