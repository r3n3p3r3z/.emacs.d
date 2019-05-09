(require 'config-package)

;; Bind M-n and M-p to navigate to the next/previous errors.
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; Install Flycheck.
(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Start it automatically for all modes except ELisp mode,
  ;; where the linter is just designed to make you mad.
  (add-hook 'find-file-hook
            (lambda ()
              (when (not (equal 'emacs-lisp-mode major-mode))
                (flycheck-mode)))))

;; Turn the modeline red when Flycheck has errors.
(use-package flycheck-color-mode-line
  :config
  (with-eval-after-load "flycheck"
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; Configure the theme.
(add-hook
 'config-appearance/dark-hook
 (lambda ()
   (with-eval-after-load "flycheck"
     (set-face-background 'flycheck-error "#660000")
     (set-face-foreground 'flycheck-error nil)
     (set-face-background 'flycheck-warning "#331800")
     (set-face-foreground 'flycheck-warning nil)
     (require 'flycheck-color-mode-line)
     (set-face-background 'flycheck-color-mode-line-error-face "#440000")
     (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
     (set-face-background 'flycheck-color-mode-line-info-face nil)
     (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
     (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
     (set-face-foreground 'flycheck-color-mode-line-info-face nil))))

(add-hook
 'config-appearance/light-hook
 (lambda ()
   (with-eval-after-load "flycheck"
     (set-face-background 'flycheck-error "#ff8888")
     (set-face-foreground 'flycheck-error nil)
     (set-face-background 'flycheck-warning "#ffcc88")
     (set-face-foreground 'flycheck-warning nil)
     (require 'flycheck-color-mode-line)
     (set-face-background 'flycheck-color-mode-line-error-face "#ff0000")
     (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
     (set-face-background 'flycheck-color-mode-line-warning-face "#886600")
     (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
     (set-face-background 'flycheck-color-mode-line-info-face nil)
     (set-face-foreground 'flycheck-color-mode-line-info-face nil))))

(use-package flycheck-pos-tip
  :demand t
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))


(with-eval-after-load "helm"
  (use-package helm-flycheck
    :bind (("C-c ! !" . helm-flycheck))))



(provide 'config-flycheck)
