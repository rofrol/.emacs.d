(use-package sgml-mode
  :defer t
  :bind (:map html-mode-map
              ("C-," . sgml-tag)
              ("C-." . sgml-close-tag)))

;; https://emacs.stackexchange.com/questions/14197/how-to-prettify-format-an-xml-buffer/31218#31218
;; (defun xml-pretty-print (beg end &optional arg)
;;   "Reformat the region between BEG and END.
;;     With optional ARG, also auto-fill."
;;   (interactive "*r\nP")
;;   (let ((fill (or (bound-and-true-p auto-fill-function) -1)))
;;     (sgml-mode)
;;     (when arg (auto-fill-mode))
;;     (sgml-pretty-print beg end)
;;     (nxml-mode)
;;     (auto-fill-mode fill)))

;; better then sgml-pretty-print
;; https://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs/4280824#4280824
;; https://stackoverflow.com/questions/25897380/pretty-print-xml-with-attribute-alignment/25897679#25897679
(defun nxml-pretty-format ()
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "xmllint --format --pretty 2 -" (buffer-name) t)))

(provide 'rofrol-xml-sgml)
