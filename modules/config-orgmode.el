(require 'config-package)

(use-package org
  :ensure org-plus-contrib
  :config
  ;; Stop org-mode from highjacking shift-cursor keys.
  (setq org-replace-disputed-keys t)
  ;; Always use visual-line-mode in org-mode, and wrap it at column 80.
  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)
     (set-visual-wrap-column 80))))

  ;; Fancy bullet rendering.
(use-package org-bullets
  :demand t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; Insert links from clipboard.
  (use-package org-cliplink
    :demand t
    :config
    (global-set-key (kbd "C-x p i") 'org-cliplink))

(use-package org-download :demand t
:init
(setq org-download-method 'attach)
(setq org-image-actual-width 600))

(use-package org-journal
  :demand t
  :init
  (setq org-journal-file-format "%Y-%m-%d.org"))

(use-package org-noter :demand t)

(use-package org-pomodoro :demand t)

(use-package org-web-tools
:demand t
:init
(setq org-web-tools-attach-archive-retry 10))

(use-package org-brain
  :demand t
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))



(provide 'config-orgmode)
