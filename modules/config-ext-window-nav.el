(defun config/previous-window ()
  (interactive)
  (-let [current (selected-window)]
    (cond
     ((eq config/--last-window current)
      (ace-select-window))

     ((window-live-p config/--last-window)
      (select-window config/--last-window))

     (t
      (ace-select-window)))
    (setq config/--last-window current)))

(defun config/select-window ()
  (interactive)
  (setq config/--last-window (selected-window))
  (ace-select-window))

(setq config/--last-window (selected-window))

(global-set-key (kbd "C-x o") 'config/previous-window)
(global-set-key (kbd "C-x C-o") 'config/select-window)
(global-set-key (kbd "C-x M-o") 'ace-swap-window)



(provide 'config-ext-window-nav)
;;; config-ext-window-nav.el ends here
