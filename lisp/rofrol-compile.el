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


(provide 'rofrol-compile)
