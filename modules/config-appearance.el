
(require 'config-package)
(require 'config-personal-taste)

(require 'term)

(use-package doom-themes :ensure t)


;; Get rid of the training wheels, if you're ready for it.
(when (not config-personal-taste/training-wheels)
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


;; Configure the light colour scheme.
(defun config-appearance/light ()
  (interactive)
  (load-theme 'doom-solarized-light)

  (run-hooks 'config-appearance/hook)
  (run-hooks 'config-appearance/light-hook))

;; Configure the dark colour scheme.
(defun config-appearance/dark ()
  (interactive)
  (load-theme 'doom-Iosvkem)

  (run-hooks 'config-appearance/hook)
  (run-hooks 'config-appearance/dark-hook))

;; Setup hooks to re-run after all modules have loaded, allowing
;; other modules to tweak the theme without having to wait
;; until they're loaded to switch to it.
(add-hook
 'config/modules-loaded-hook
 (lambda ()
   (run-hooks 'config-appearance/hook)
   (cond
    ((equal config-personal-taste/style 'dark)
     (run-hooks 'config-appearance/dark-hook))
    ((equal config-personal-taste/style 'light)
     (run-hooks 'config-appearance/light-hook)))))

;; Maximise the Emacs frame if that's how you like it.
(set-frame-parameter nil 'fullscreen 'maximized)

(use-package doom-modeline :ensure t
  :init
(doom-modeline-mode 1))


;; Don't defer screen updates when performing operations.
(setq redisplay-dont-pause t)

;; When not in a terminal, configure a few window system specific things.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Show line numbers in buffers.
(global-linum-mode  t)
(setq linum-format (if (not window-system) "%4d " "%4d"))

;; Highlight the line number of the current line.
(use-package hlinum
  :config
  (hlinum-activate))

;; Show column numbers in modeline.
(setq column-number-mode t)

;; Show current function in modeline.
(which-function-mode)


;; Highlight matching braces.
(show-paren-mode 1)

;; Unclutter the modeline
(use-package diminish)

(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "js2-highlight-vars" '(diminish 'js2-highlight-vars-mode))
(eval-after-load "mmm-mode" '(diminish 'mmm-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "auto-indent-mode" '(diminish 'auto-indent-minor-mode))
;; (eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "cider" '(diminish 'cider-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))

(eval-after-load "js2-mode"
  '(defadvice js2-mode (after js2-rename-modeline activate)
     (setq mode-name "JS+")))
(eval-after-load "clojure-mode"
  '(defadvice clojure-mode (after clj-rename-modeline activate)
     (setq mode-name "Clj")))
(eval-after-load "typescript"
  '(defadvice typescript-mode (after typescript-rename-modeline activate)
     (setq mode-name "TS")))
(eval-after-load "nxhtml-mode"
  '(defadvice nxhtml-mode (after nxhtml-rename-modeline activate)
     (setq mode-name "HTML")))
(eval-after-load "js"
  '(defadvice js-mode (after js-rename-modeline activate)
     (setq mode-name "JS")))
(defadvice emacs-lisp-mode (after elisp-rename-modeline activate)
  (setq mode-name "ELisp"))

;; Handle ANSI colours in compile buffer output.
;; From https://gist.github.com/jwiegley/8ae7145ba5ce64250a05
(defun compilation-ansi-color-process-output ()
  (ansi-color-process-output nil)
  (set (make-local-variable 'comint-last-output-start)
       (point-marker)))
(add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output)

;; Install the colour scheme according to personal taste.
(defun config-appearance/apply-style ()
  (interactive)
  (cond
   ((equal config-personal-taste/style 'dark)
    (config-appearance/dark))
   ((equal config-personal-taste/style 'light)
    (config-appearance/light))))

(config-appearance/apply-style)

(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
             all-the-icons-install-fonts)
  :init
  (defun my/*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))

  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                 all-the-icons-faicon all-the-icons-fileicon
                 all-the-icons-wicon all-the-icons-alltheicon))
     (advice-add fn :around #'my/*disable-all-the-icons-in-tty)))




(provide 'config-appearance)
;;; config-appearance.el ends here
