(load-file "~/.emacs.d/lisp/screencast-mode.el")

(require 'screencast)
(setq screencast-speed 1.2)

(define-derived-mode screencast-mode nil "screencast"
  "Major mode for viewing screencasts."
  (org-mode)
  (auto-fill-mode 1)
  )





(provide 'config-screencast)
