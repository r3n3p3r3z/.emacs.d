(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq ;; Always copy/delete recursively
      dired-recursive-copies  'always
      dired-recursive-deletes 'top
      ;; files
      image-dired-dir (concat user-emacs-directory "image-dired/")
      image-dired-db-file (concat user-emacs-directory "image-dired/db.el")
      image-dired-gallery-dir (concat user-emacs-directory "gallery/")
      image-dired-temp-image-file (concat user-emacs-directory "temp-image")
      image-dired-temp-rotate-image-file (concat user-emacs-directory "temp-rotate-image"))


(use-package dired-k
  :after dired
  :config
  (setq dired-k-style 'git)

  (defun +dired*dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired*dired-k-highlight)

  (add-hook 'dired-initial-position-hook #'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))


(use-package stripe-buffer
  :commands stripe-buffer-mode
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))


;; A function for deleting the file being edited.
;; This one is a bit dangerous, even with the yes/no question, so
;; it's not bound to any key by default.
;; Run it using M-x delete-current-buffer-file.
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; And a function for renaming the file being edited, bound to C-x C-r.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)



(provide 'config-dired)
;;; config-dired.el ends here
