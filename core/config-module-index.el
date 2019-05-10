(setq custom-safe-themes t)

(setq
 core-available-modules
 '((config-appearance "how Emacs looks" :recommended)
   (config-dashboard "a dashboard for emacs" :recommended)
   (config-fonts "adjust font size on the fly" :recommended)
   (config-help "ways to get more help" :recommended)
   (config-screencast "create and view interactive screencast" :recommended)
   (config-general "basic editor settings" :recommended)
   (config-navigation "moving around better" :recommended)
   (config-ext-window-nav "alternative window navigation" :recommended)
   (config-editing "editing improvements" :recommended)
   (config-complete "auto completion" :recommended)
   (config-orgmode "your personal everything manager" :recommended)
   (config-snippets "snippet management" :recommended)
   (config-elisp "Emacs Lisp" :recommended)
   (config-helm "advanced selection and narrowing" :recommended)
   (config-dired "enhanced file manager" :recommended)
   (config-writeroom-mode "Minor mode for distraction-free writing" :recommended)
   (config-flycheck "run linters automatically with Flycheck" :recommended)
   (config-markdown "Markdown support" :recommended)
   (config-codestyle "code formatting, whitespace management" :recommended)
   (config-nov "featureful EPUB reader mode" :optional)
   (config-deft "quickly browse, filter, and edit plain text notes" :optional)
   (config-howm "write wiki-like fragmentary notes, and read collectively" :optional)
   (config-hyperbole "Hypertextual Information Manager" :optional)
   (config-git "Git tools" :optional)
   (config-project "manage projects with Projectile" :optional)
   (config-pdf-tools "powerful pdf viewer" :optional)
   (config-eshell "the native Emacs shell" :optional)
   (config-refactor "easy access to refactoring tools" :optional)
   (config-emoji "display Unicode emoji even if your system doesn't" :optional)
   (config-lsp "Language Server Protocol" :optional)
   (config-html "HTML, CSS and friends" :optional)
   (config-javascript "JavaScript language support" :optional)
   (config-js-web-mode "alternative JS support using web-mode" :optional)

   (config-experimental "playground for experimenting with new modules" :experimental)


))

(require 'cl)
(defcustom core-modules (mapcar #'car
                                (remove-if-not
                                 (lambda (i) (equal :recommended (caddr i)))
                                 core-available-modules))
  "Your choice of Emacs modules.")

(defun core-load-modules ()
  (interactive)
  (dolist (module core-modules) (require module nil t))
  (run-hooks 'core-modules-loaded-hook))


(provide 'config-module-index)
;;; config-module-index.el ends here
