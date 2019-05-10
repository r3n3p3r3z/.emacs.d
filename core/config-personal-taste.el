(defgroup core-emacs nil
  "Your personal taste in Config Emacs."
  :prefix "core-personal-taste/")

(defcustom core-personal-taste/run-wizard t
  "Should we run the Config Emacs startup wizard on the next startup?"
  :group 'core-emacs
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom core-personal-taste/window-state 'maximised
  "Should Emacs maximise its frame on startup, or leave it alone?"
  :group 'core-emacs
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Maximise" maximised)))

(defcustom core-personal-taste/style 'dark
  "Light or dark colour scheme?"
  :group 'core-emacs
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark" dark)))

(defcustom core-personal-taste/training-wheels t
  "Would you prefer an Emacs experience without the clutter of the menu bar,
toolbar and scrollbar?"
  :group 'core-emacs
  :type '(choice (const :tag "Yes, please" t)
                 (const :tag "I'm not ready for that" nil)))


(defcustom core-auto-save t
  "Non-nil values enable User's auto save."
  :type 'boolean
  :group 'core-emacs)

(defcustom user-core-init-file (expand-file-name "personal/"
                                                    user-emacs-directory)
  "Path to your personal customization file.
User recommends you only put personal customizations in the
personal folder.  This variable allows you to specify a specific
folder as the one that should be visited when running
`crux-find-core-init-file'.  This can be easily set to the desired buffer
in lisp by putting `(setq core-core-init-file load-file-name)'
in the desired elisp file."
  :type 'string
  :group 'core-emacs)






(provide 'config-personal-taste)
;;; core-personal-taste.el ends here
