 (use-package key-chord
   :disabled
   :config
   (key-chord-mode 1)
   ;; (let (
   (key-chord-define-global "jj" 'avy-goto-word-1)
   (key-chord-define-global "jl" 'avy-goto-line)
   (key-chord-define-global "jk" 'avy-goto-char)
   (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
   (key-chord-define-global "uu" 'undo-tree-visualize)
   (key-chord-define-global "xx" 'counsel-M-x)
   (key-chord-define-global "yy" 'counsel-yank-pop)
   (key-chord-define-global "  " ". ")

       ;; Max time delay between two key presses to be considered a key chord
    (setq key-chord-two-keys-delay 0.1) ; default 0.1

    ;; Max time delay between two presses of the same key to be considered a key chord.
    ;; Should normally be a little longer than `key-chord-two-keys-delay'.
    (setq key-chord-one-key-delay 0.2) ; default 0.2


      ;; This create a prefix keymap
   ;; https://stackoverflow.com/questions/25473660/how-do-i-use-a-key-chord-combination-as-a-prefix-binding
   (let
       ((sub-keymap (make-sparse-keymap)))
     (define-key sub-keymap "a" 'org-agenda)
     (define-key sub-keymap "c" 'org-capture)
     (key-chord-define-global "cc" sub-keymap))

   )

(provide 'config-experimental)
