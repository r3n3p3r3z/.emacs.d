(use-package exec-path-from-shell :demand t)

(exec-path-from-shell-initialize)

;; It's all in the Meta
(setq ns-function-modifier 'hyper)

(defun user-swap-meta-and-super ()
  "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (message "Command is now bound to SUPER and Option is bound to META.")))

(define-key user-mode-map (kbd "C-c w") 'user-swap-meta-and-super)
(define-key user-mode-map (kbd "s-/") 'hippie-expand)

;; There's no point in hiding the menu bar on macOS, so let's not do it
(menu-bar-mode +1)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


(provide 'config-macos)
;;; user-macos.el ends here
