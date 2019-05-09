(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs writes backup files to `filename~` by default. This is messy,
;; so let's tell it to write them to `~/.emacs.d/bak` instead.
;; If you have an accident, check this directory - you might get lucky.
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir ".bak")))))

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

(use-package vlf :defer t)

(use-package scratch :demand t)

(use-package better-defaults :ensure t)

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default
    visual-fill-column-center-text t
    visual-fill-column-width
    ;; take Emacs 26 line numbers into account
    (+ (if (boundp 'display-line-numbers) 6 0)
       fill-column)))


;; If available, use `xdg-open' to open URLs.
(when (config/is-exec "xdg-open")
  (setq-default
   browse-url-browser-function (quote browse-url-generic)
   browse-url-generic-program "xdg-open"))

;; Make compilation buffers scroll to follow the output, but stop scrolling
;; at the first error.
(setq compilation-scroll-output 'first-error)

(provide 'config-general)
