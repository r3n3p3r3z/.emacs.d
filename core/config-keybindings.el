(require 'easymenu)
(require 'imenu-anywhere)
(require 'crux)

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'crux-open-with)
    (define-key map (kbd "C-c g") 'config-google)
    (define-key map (kbd "C-c G") 'config-github)
    (define-key map (kbd "C-c y") 'config-youtube)
    (define-key map (kbd "C-c U") 'config-duckduckgo)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map (kbd "C-a") 'crux-move-beginning-of-line)
    (define-key map [(shift return)] 'crux-smart-open-line)
    (define-key map (kbd "M-o") 'crux-smart-open-line)
    (define-key map [(control shift return)] 'crux-smart-open-line-above)
    (define-key map [(control shift up)]  'move-text-up)
    (define-key map [(control shift down)]  'move-text-down)
    (define-key map [(meta shift up)]  'move-text-up)
    (define-key map [(meta shift down)]  'move-text-down)
    (define-key map (kbd "C-c n") 'crux-cleanup-buffer-or-region)
    (define-key map (kbd "C-c f")  'crux-recentf-find-file)
    (define-key map (kbd "C-M-z") 'crux-indent-defun)
    (define-key map (kbd "C-c u") 'crux-view-url)
    (define-key map (kbd "C-c e") 'crux-eval-and-replace)
    (define-key map (kbd "C-c s") 'crux-swap-windows)
    (define-key map (kbd "C-c D") 'crux-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'crux-visit-term-buffer)
    (define-key map (kbd "C-c k") 'crux-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'crux-find-config-init-file)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    (define-key map (kbd "C-c i") 'imenu-anywhere)
    ;; extra prefix for projectile
    (define-key map (kbd "s-p") 'projectile-command-map)
    (define-key map (kbd "C-c p") 'projectile-command-map)
    ;; make some use of the Super key
    (define-key map (kbd "s-r") 'crux-recentf-find-file)
    (define-key map (kbd "s-j") 'crux-top-join-line)
    (define-key map (kbd "s-k") 'crux-kill-whole-line)
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-log-buffer-file)
    (define-key map (kbd "s-m b") 'magit-blame)
    (define-key map (kbd "s-o") 'crux-smart-open-line-above)
    (easy-menu-define my-mode-menu map
      "User's menu."
      '("User"
        ("Files"
         ["Open with..." crux-open-with]
         ["Re-open as root" crux-reopen-as-root]
         ["Delete file and buffer" crux-delete-file-and-buffer]
         ["Rename buffer and file" crux-rename-buffer-and-file]
         ["Find init file" crux-find-config-init-file]
         ["Find custom file" crux-find-config-custom-file]
         ["Find shell config file" crux-find-shell-init-file])
        ("Buffers"
         ["Clean up buffer or region" crux-cleanup-buffer-or-region]
         ["Kill other buffers" crux-kill-other-buffers])
        ("Editing"
         ["Go to beginning of line" crux-move-beginning-of-line]
         ["Kill line" crux-smart-kill-line]
         ["Kill whole line" crux-kill-whole-line]
         ["Insert empty line below" crux-smart-open-line]
         ["insert empty line above" crux-smart-open-line-above]
         ["Move up" move-text-up]
         ["Move down" move-text-down]
         ["Duplicate line or region" crux-duplicate-current-line-or-region]
         ["Indent rigidly and copy to clipboard" crux-indent-rigidly-and-copy-to-clipboard]
         ["Indent defun" crux-indent-defun]
         ["Insert date" crux-insert-date]
         ["Eval and replace" crux-eval-and-replace])
        ("Windows"
         ["Swap windows" crux-swap-windows])
        ("General"
         ["Visit term buffer" crux-visit-term-buffer]
         ["Search in Google" config-google]
         ["View URL" crux-view-url])))
    map)
  "Keymap for User mode.")

;; define minor mode
(define-minor-mode my-mode
  "Minor mode to consolidate Emacs User extensions.

\\{config-mode-map}"
  :lighter " Pre"
  :keymap my-mode-map
  :global t)



(require 'view)

(define-key view-mode-map (kbd "e") 'View-scroll-half-page-forward)
  (define-key view-mode-map (kbd "u") 'View-scroll-half-page-backward)

  ;; less like
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
  (define-key view-mode-map (kbd "g") 'View-goto-line)
  (define-key view-mode-map (kbd "G") 'View-goto-line-last)
  ;; vi/w3m like
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char)



;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'smex)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-varable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; replace zap-to-char functionality with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'user-fullscreen))

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)

;; improved window navigation with ace-window
(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(provide 'config-keybindings)

;;; user-global-keybindings.el ends here
