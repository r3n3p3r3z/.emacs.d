(require 'config-package)

;; Install Projectile and activate it for all things.
;; Learn about Projectile: http://batsov.com/projectile/
(use-package projectile
  :demand t
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  ;; Use C-c C-f to find a file anywhere in the current project.
  :bind ("C-c C-f" . projectile-find-file)
  :diminish projectile-mode)

;; Use ibuffer instead of list-buffers (C-x C-b) and sort by project.
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(provide 'config-project)
