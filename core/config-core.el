(require 'thingatpt)
(require 'cl-lib)


(defvar core/tips
  '(""
    ))


(use-package f)
(use-package s)
(use-package dash)



(defun core/buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun core/search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro core/install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "core/%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (core/search ,search-engine-url ,search-engine-prompt)))

(core/install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(core/install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(core/install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(core/install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(defun core/recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory core/dir 0))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 10000)

(defun core/tip-of-the-day ()
  "Display a random entry from `core/tips'."
  (interactive)
  (when (and core/tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "Tip: " (nth (random (length core/tips)) core/tips)))))

(defun core/eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;;; Emacs in macOS already has fullscreen support
;;; Emacs has a similar built-in command in 24.4
(defun core/fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(defun core/wrap-with (s)
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
;;; config-core ends here
