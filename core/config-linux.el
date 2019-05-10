
;; On Linux Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(use-package exec-path-from-shell)

(exec-path-from-shell-initialize)

(provide 'config-linux)
;;; user-linux.el ends here
