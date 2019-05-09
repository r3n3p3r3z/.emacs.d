(use-package emr
  :config
  (add-hook 'prog-mode-hook 'emr-initialize)
  ;; Just hit M-RET to access your refactoring tools in any supported mode.
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu))

(provide 'config-refactor)
;;; config-refactor.el ends here
