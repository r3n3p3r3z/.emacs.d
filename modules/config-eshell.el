(require 'config-lib)

;; Define a keybinding to get to your eshell quickly.
(global-set-key (kbd "C-c e") 'eshell)

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; Suggest alternatives for mistyped commands.
;; (use-package eshell-did-you-mean
;;   :config
;;   (eshell-did-you-mean-setup))

;; Define a pretty prompt.
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Disable company-mode for eshell, falling back to pcomplete,
;; which feels more natural for a shell.
(add-hook 'eshell-mode-hook
          (lambda ()
            (company-mode 0)))

;; When completing with multiple options, complete only as much as
;; possible and wait for further input.
(setq eshell-cmpl-cycle-completions nil)

;; esh-autosuggest provides fish shell like autosuggestion from history.
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; Extend pcomplete with smart completion provided by the fish shell, or
;; bash if fish isn't available.
(use-package bash-completion
  :if (config/is-exec "bash")
  :commands bash-completion-dynamic-complete
  :init
  (add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete)
  (setq fish-completion-fallback-on-bash-p t))
(use-package fish-completion
  :if (config/is-exec "fish")
  :config
  (global-fish-completion-mode))



(provide 'config-eshell)
;;; config-eshell.el ends here
