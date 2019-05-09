(require 'config-package)
(require 'config-lib)
(require 'config-json)

;; If npm is installed, add its local prefix to the executable
;; search path, which helps Emacs find linters etc.
;; This isn't Windows compatible.
(-when-let (npm-prefix (config/exec-if-exec "npm" "config get prefix"))
  (setenv "PATH" (concat npm-prefix "/bin:" (getenv "PATH"))))

;; Use web-mode for all JS files.
(use-package web-mode
  :mode (("\\.jsx?$" . web-mode)
         ("\\.es6\\'" . web-mode)
         ("\\.ejs\\'" . web-mode))
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.jsx?$")))
  ;; Stop web-mode from using block comments in comment-dwim.
  (setq web-mode-comment-formats
        (-map-when (lambda (i) (equal (car i) "javascript"))
                   (lambda (i) (cons (car i) "//"))
                   web-mode-comment-formats))
  (add-to-list 'web-mode-comment-formats `("jsx" . "//"))

  ;; Let Flycheck know that we're using web-mode for JS.
  (with-eval-after-load "flycheck"
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (setq flycheck-javascript-eslint-executable (or (config/resolve-exec "eslint") "eslint"))))

;; Set up LSP support if the LSP module is loaded.
(with-eval-after-load "config-lsp"
  (use-package lsp-javascript-typescript
    :hook (web-mode . lsp-javascript-typescript-enable)))



(provide 'config-js-web-mode)
