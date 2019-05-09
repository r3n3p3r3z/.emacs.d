(require 'config-personal-taste)


(defun config-startup-wizard/get-style ()
  (x-popup-dialog
   t '("Light or dark background?"
       ("Light" . light)
       ("Dark" . dark))))


(defun config-startup-wizard ()
  (interactive)

  (x-popup-dialog
   t '("Welcome to the Church of Emacs, my child!

This is the first time you've run it, so let's
start off by asking you some basic questions
about how you would like to use emacs.

If you change your mind about any of these
decisions, you can re-run this wizard with
`M-x config-startup-wizard` that is Alt+X
config-startup-wizard <enter>.

"
       ("I am ready to emacs" . t)) t)

  (customize-save-variable
   'config-personal-taste/style
   (config-startup-wizard/get-style))

  (customize-save-variable 'config-personal-taste/run-wizard nil)
  (setq config/wizard-did-run t)
  (config/select-modules))

(when config-personal-taste/run-wizard
  (config-startup-wizard))



(provide 'config-startup-wizard)
