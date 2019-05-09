(require 'config-package)

;; which-key prompts you with available options when you type a partial
;; command sequence. Try it out: hit C-x and just wait for two seconds.
(use-package which-key
  :commands which-key-mode
  :demand t
  :config
  (which-key-mode)
  ;; Set the delay before which-key appears.
  (setq-default which-key-idle-delay 0.0)
  ;; which-key will truncate special keys by default, eg. SPC turns into
  ;; an orange D. Turn this off to avoid confusion.
  (setq-default which-key-special-keys nil)
  ;; Hit C-h C-k to have which-key show you all top level key bindings.
  :bind ("C-h C-k" . which-key-show-top-level)
  :diminish which-key-mode)

;; Get an instant cheat sheet for your current major mode
;; with C-h C-m.
(use-package discover-my-major
  :commands (discover-my-major discover-my-mode)
  :bind ("C-h C-m" . discover-my-major))



(provide 'config-help)
