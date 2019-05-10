(require 'config-personal-taste)


(defun core-startup-wizard/get-style ()
  (x-popup-dialog
   t '("Light or dark background?"
       ("Light" . light)
       ("Dark" . dark))))


(defun core-startup-wizard ()
  (interactive)

  (x-popup-dialog
   t '("Welcome to the Church of Emacs, my child!

This is the first time you've run it, so let's
start off by asking you some basic questions
about how you would like to use emacs.

If you change your mind about any of these
decisions, you can re-run this wizard with
`M-x core-startup-wizard` that is Alt+X
core-startup-wizard <enter>.

"
       ("I am ready to EMACS!!!" . t)) t)

  (customize-save-variable
   'core-personal-taste/style
   (core-startup-wizard/get-style))

  (customize-save-variable 'core-personal-taste/run-wizard nil)
  (setq core-wizard-did-run t)
  (core-select-modules))

(when core-personal-taste/run-wizard
  (core-startup-wizard))



(provide 'config-startup-wizard)
