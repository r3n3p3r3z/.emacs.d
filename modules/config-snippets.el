(require 'config-package)

;; The s.el package contains a lot of functions useful in snippets.
(use-package s)

;; Install yasnippet and make it available globally.
;; Read about it here: http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  ;;:commands yas-global-mode
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

(use-package auto-yasnippet
  :commands (aya-create aya-expand aya-open-line aya-persist-snippet)
  :config
  (setq aya-persist-snippets-dir (concat user-emacs-directory "auto-snippets/")))


(provide 'config-snippets)
;;; config-snippets.el ends here
