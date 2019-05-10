(defgroup config-emacs nil
  "Your personal taste in Config Emacs."
  :prefix "config-personal-taste/")

(defcustom config-personal-taste/run-wizard t
  "Should we run the Config Emacs startup wizard on the next startup?"
  :group 'config-emacs
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom config-personal-taste/window-state 'maximised
  "Should Emacs maximise its frame on startup, or leave it alone?"
  :group 'config-emacs
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Maximise" maximised)))

(defcustom config-personal-taste/style 'dark
  "Light or dark colour scheme?"
  :group 'config-emacs
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark" dark)))

(defcustom config-personal-taste/training-wheels t
  "Would you prefer an Emacs experience without the clutter of the menu bar,
toolbar and scrollbar?"
  :group 'config-emacs
  :type '(choice (const :tag "Yes, please" t)
                 (const :tag "I'm not ready for that" nil)))


(defcustom config-auto-save t
  "Non-nil values enable User's auto save."
  :type 'boolean
  :group 'config-emacs)

(defcustom user-config-init-file (expand-file-name "personal/"
                                                    user-emacs-directory)
  "Path to your personal customization file.
User recommends you only put personal customizations in the
personal folder.  This variable allows you to specify a specific
folder as the one that should be visited when running
`crux-find-config-init-file'.  This can be easily set to the desired buffer
in lisp by putting `(setq config-config-init-file load-file-name)'
in the desired elisp file."
  :type 'string
  :group 'config-emacs)






(provide 'config-personal-taste)
;;; config-personal-taste.el ends here
