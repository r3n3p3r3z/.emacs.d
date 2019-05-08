(use-package deft
  :commands (deft)
  :init
  (setq 
   deft-extensions '("org" "md" "txt")
   deft-default-extension "org"
   deft-use-filename-as-title t))

(provide 'config-deft)
