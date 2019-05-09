(require 'thingatpt)
(require 'cl-lib)

(use-package crux :ensure t)
(use-package editorconfig :ensure t)
(use-package epl :ensure t)

(defun config-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun config-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro config-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "config-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (config-search ,search-engine-url ,search-engine-prompt)))

(config-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(config-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(config-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(config-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(defun config-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory config-dir 0))

(defvar config-tips
  '(""
    ))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 10000)

(defun config-tip-of-the-day ()
  "Display a random entry from `config-tips'."
  (interactive)
  (when (and config-tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "Tip: " (nth (random (length config-tips)) config-tips)))))

(defun config-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(require 'epl)



(defun config-update ()
  "Update User to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating User...")
    (cd config-dir)
    (shell-command "git pull")
    (config-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun config-update-packages (&optional arg)
  "Update User's packages.
This includes package installed via `require-package'.

With a prefix ARG updates all installed packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update User's packages? ")
    (if arg
        (epl-upgrade)
      (epl-upgrade (cl-remove-if-not (lambda (p) (memq (epl-package-name p) config-packages))
                                     (epl-installed-packages))))
    (message "Update finished. Restart Emacs to complete the process.")))

;;; Emacs in macOS already has fullscreen support
;;; Emacs has a similar built-in command in 24.4
(defun config-fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(defun config-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(setq compilation-always-kill t)
(setq compilation-ask-about-save nil)
(add-hook 'compilation-filter-hook
          (lambda ()
            (when (eq major-mode 'compilation-mode)
              (require 'ansi-color)
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region (point-min) (point-max))))))


(provide 'config-core)

