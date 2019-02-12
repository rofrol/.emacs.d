;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(electric-pair-mode t)

;; https://emacs.stackexchange.com/questions/12269/getting-electric-pair-mode-to-behave-more-nicely-around-existing-strings/14836#14836
(setq-default electric-pair-inhibit-predicate
              (lambda (c)
                (if (looking-at "\\([ \t]\\|$\\)")
                    (electric-pair-default-inhibit c)
                    t)))

(provide 'rofrol-electric)
