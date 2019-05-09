(require 'config-lib)
(require 'widget)
(require 'cus-edit)

(defun config/select-modules ()
  "Select the modules Config Emacs should load on startup."
  (interactive)

  (switch-to-buffer "*Config Emacs Modules*")
  (kill-all-local-variables)
  (-let [inhibit-read-only t] (erase-buffer))
  (remove-overlays)

  (setq-local selected-modules config/modules)

  (-let ((save-settings
          (lambda (&rest ignore)
            (interactive)
            (customize-save-variable 'config/modules selected-modules)
            (package-refresh-contents)
            (config/load-modules)
            (kill-buffer))))

    (widget-insert (propertize "Config Emacs Modules" 'face 'custom-group-tag))
    (widget-insert "\n")

    (widget-insert "
This menu allows you to select feature modules for your Config Emacs.

Navigate between checkboxes using <tab> and S-<tab>, or use the cursor
keys to move around. Hit <return> to toggle checkboxes, or to press the
buttons. When you're done, press the `Save' or `Cancel' buttons, or just
save the buffer (C-x C-s).

You can also use your mouse, but you must resist the urge to do this.
An Emacs Master does not use mice.
")

    (widget-insert "\n  ")
    (widget-create 'push-button
                   :tag "Save"
                   :notify save-settings)
    (widget-insert "  ")
    (widget-create 'push-button :tag "Cancel"
                   :notify (lambda (&rest ignore) (kill-buffer)))

    (widget-insert "\n\n  ")
    (apply 'widget-create 'checklist
           :indent 2
           :greedy t
           :value selected-modules
           :notify (lambda (this &rest ignore)
                     (setq-local selected-modules (widget-value this)))
           (-map (lambda (mod)
                   (-let [(sym desc) mod]
                     `(item :tag ,(s-concat (s-pad-right 24 " " (symbol-name sym)))
                            :doc ,desc
                            :value ,sym
                            :format "%t %d")))
                 config/available-modules))

    (widget-insert "\n")

    (use-local-map (copy-keymap widget-keymap))
    (local-set-key (kbd "C-x C-s") save-settings)

    (widget-setup)
    (widget-forward 1)))



(provide 'config-module-selector)
;;; config-module-selector.el ends here
