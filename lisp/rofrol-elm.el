;; -*- lexical-binding: t; -*-

;; required for company-elm
(add-hook 'after-init-hook 'global-company-mode)

(use-package elm-mode
    ;; elm-compile-add-annotations is the function to be fixed for this.
    ;; in fact the issue here is that the new `elm make` does not support the `--warn` flag,
    ;; and this is what elm-mode was using previously in order to have `elm make` print out
    ;; the missing annotation so it could be inserted. Without support for that,
    ;; there's no obvious alternative route to providing this functionality.
    ;; https://github.com/jcollard/elm-mode/issues/152#issuecomment-450596903

    :mode ("\\.elm\\'" . elm-mode)
    :straight t
    :init

    ;; for elm-0.19
    (setq elm-package-json "elm.json")

    ;;For Windows https://github.com/jcollard/elm-mode/issues/135
    ;;on Linux `setq elm-tags-on-save t` should be sufficient.
    ;;I should test it

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
      (when (rofrol/elm--has-dependency-file)
	(let* ((default-directory (rofrol/elm--find-dependency-file-path))
	       (ctags-command "rg --files -telm | ctags -e -L -"))
	  (call-process-shell-command (concat ctags-command "&") nil 0))))

    (defun init-elm-mode ()
      "Disable electric-indent-mode and let indentation cycling feature work"
      (if (fboundp 'electric-indent-local-mode)
	  (electric-indent-local-mode -1))
      (add-to-list 'company-backends 'company-elm)
      (setq tags-revert-without-query 1)
      (elm-format-on-save-mode)
      ;;(setq elm-tags-on-save t))
      (add-hook 'after-save-hook 'rofrol/elm-mode-generate-tags))

    (add-hook 'elm-mode-hook 'init-elm-mode)

    ;; needs to be at top-level https://github.com/jcollard/elm-mode/issues/129#issuecomment-346974494
    (with-eval-after-load 'elm-mode
      (remove-hook 'elm-mode-hook 'elm-indent-mode))

    :bind
	("<f5>" . elm-occur-toggle)
	("<f9>" . elm-beginning-of-defun)
	("<f10>" . elm-end-of-defun))

;; depends on rofrol-occur
(defun elm-occur ()
 "Elm and occure searching for lines with definitions and annotations"
 (interactive)
 ;; (occur "^[a-z].*\\(:.+$\\|=$\\)"))
 ;; (occur "^[a-z].*=$"))
 ;; non-gready +? https://stackoverflow.com/questions/2217928/how-do-i-write-a-regular-expression-that-excludes-rather-than-matches-e-g-not/15714513#15714513
 ;; match new line https://stackoverflow.com/questions/1309050/emacs-query-replace-regexp-multiline/1309092#1309092
 (occur "^\\([a-z].*=\\|type alias +\\(\n +[A-Z]\\)\\| +[{,] [a-z][a-zA-Z0-9_]* :.*\\(\n +}\\)?$\\|[a-z]+ :\\|type \\| +[=|] [a-zA-Z ().,]*$\\|port .*:\\| +(?case .* of$\\| +(?case\n\\(.*\n\\)+? +of$\\| +.*+ ->$\\| *-- BOOKMARK\\|-- \\|.*Debug\\.log\\)"))

(defun elm-occur-toggle ()
  (interactive)
  (let ((buffers (seq-filter (lambda (window)
		       (string-prefix-p "*Occur: " (buffer-name (window-buffer window))))
		  (window-list))))
    (if (not (eq 0 (length buffers)))
	(kill-buffer (window-buffer (car buffers)))
      (elm-occur)
      (occur-rename-buffer))))

(provide 'rofrol-elm)
