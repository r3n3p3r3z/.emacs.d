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



(provide 'config-personal-taste)
;;; config-personal-taste.el ends here
