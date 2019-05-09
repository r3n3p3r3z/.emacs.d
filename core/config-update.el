(defun config/update ()
  (interactive)
  (let ((default-directory dotfiles-dir)
        (buf (get-buffer-create "*config-emacs update*")))
    (switch-to-buffer-other-window buf)
    (shell-command "git pull --ff-only --stat" buf)
    (end-of-buffer)
    (insert "\nRun `M-x config/select-modules' to review and install new modules.\n")
    (local-set-key (kbd "q") 'quit-window)))

(provide 'config-update)
;;; config-update.el ends here
