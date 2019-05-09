(require 'config-package)

;; Install Markdown support.
(use-package markdown-mode
  :commands markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))


  (provide 'config-markdown)
