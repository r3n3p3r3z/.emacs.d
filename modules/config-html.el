(require 'config-package)
(require 'config-editing)

;; web-mode is a special mode for HTML which copes with embedded JS/CSS,
;; JSX, various templating systems, etc.
;; Learn about web-mode: http://web-mode.org/
(use-package web-mode
  :mode (;; We'd like to use web-mode for HTML, instead of the default html-mode.
         ("\\.html?\\'" . web-mode)
         ;; Let's add some extensions from the web-mode docs too.
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  ;; Highlight the element under the cursor.
  (setq-default web-mode-enable-current-element-highlight t)
  ;; Key for renaming tags
  (bind-keys :map web-mode-map
             ("C-c C-r" . 'mc/mark-sgml-tag-pair)))

;; Colourise colour names in certain modes.
(use-package rainbow-mode
  :config
  (dolist (mode '(css-mode less-css-mode html-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (rainbow-mode))))
  :diminish rainbow-mode)


(provide 'config-html)
