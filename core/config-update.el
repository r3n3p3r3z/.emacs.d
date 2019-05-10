(defun config/update ()
  (interactive)
  (let ((default-directory user-emacs-directory)
        (buf (get-buffer-create "*Emacs Update*")))
    (switch-to-buffer-other-window buf)
    (shell-command "git pull --ff-only --stat" buf)
    (end-of-buffer)
    (insert "\nRun `M-x core-select-modules' to review and install new modules.\n")
    (local-set-key (kbd "q") 'quit-window)))

(provide 'config-update)
;;; config-update.el ends here
