;; -*- lexical-binding: t; -*-

(add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

(provide 'rofrol-dired)
