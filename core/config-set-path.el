(paradox-require 'exec-path-from-shell)

(when (memq window-system '(x mac ns))
  (exec-path-from-shell-initialize))

(provide 'config-set-path)
;;; config-set-path.el ends here
