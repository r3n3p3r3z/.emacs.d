  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner ()  )
;  (add-to-list 'dashboard-items '(agenda) t)
  (setq dashboard-items '())

(add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
(add-to-list 'dashboard-items '(custom) t)

    (defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)
(defun dashboard-insert-custom (list-size)
  (let ((all-the-icons-scale-factor 2.00)
        (all-the-icons-default-adjust -0.00))
    (mapc (lambda (btn)
            (when btn
              (cl-destructuring-bind (label icon fn) btn
                (insert
                 (with-temp-buffer
                   (insert-text-button
                    (concat (all-the-icons-octicon icon :face 'font-lock-keyword-face)
                            (propertize (concat " " label) 'face 'font-lock-keyword-face))
                    'action `(lambda (_) ,fn)
                    'follow-link t)
                   (dashboard-center (- dashboard--width 2) (buffer-string)))
                 "\n\n"))))
          `( ("Open project" "briefcase"
             (call-interactively (or (command-remapping #'projectile-switch-project)
                                     #'projectile-switch-project)))
             ("Recently opened files" "file-text"
             (call-interactively (or (command-remapping #'helm-recentf)
                                     #'helm-recentf)))

             ("Bookmarks" "bookmark"
             (call-interactively (or (command-remapping #'bookmark-jump)
                                     #'bookmark-jump)))

             ,(when (fboundp 'org-agenda-list)
             '("Agenda for this week" "calendar"
               (call-interactively #'org-agenda-list)))

             
             ("Capture" "comment"
              (call-interactively (or (command-remapping #'org-capture)
                                      #'org-capture)))
          ))))


(defvar dashboard--width 80)
(defvar dashboard--height 0)
(defvar dashboard--old-fringe-indicator fringe-indicator-alist)
(defun dashboard-center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))


(defun avy-dashboard-button (char &optional arg)
    "Jump to the currently visible CHAR.
  The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))

    (avy-with avy-goto-char
      (avy-jump
       (if (= 13 char)
           "\n"
         (regexp-quote (string char)))
       :window-flip arg))
    (push-button arg))

(define-key dashboard-mode-map "n" #'dashboard/next-button)
(define-key dashboard-mode-map "p" #'dashboard/previous-button)
(define-key dashboard-mode-map "j" #'dashboard/next-button)
(define-key dashboard-mode-map "k" #'dashboard/previous-button)
(define-key dashboard-mode-map "[" #'dashboard/next-button)
(define-key dashboard-mode-map "]" #'dashboard/previous-button)

(define-key dashboard-mode-map [right] #'dashboard/next-button)
(define-key dashboard-mode-map [left] #'dashboard/previous-button)
(define-key dashboard-mode-map [down] #'dashboard/next-button)
(define-key dashboard-mode-map [up] #'dashboard/previous-button)

(define-key dashboard-mode-map "l" #'dashboard/last-buton)
(define-key dashboard-mode-map "f" #'dashboard/first-button)


(define-key dashboard-mode-map "s" #'org-save-all-org-buffers)
(define-key dashboard-mode-map [tab] #'avy-dashboard-button)




(define-derived-mode dashboard-mode special-mode
  (format "Dashboard")
  "Major mode for the BMACS dashboard buffer."
  (read-only-mode +1)
  (global-linum-mode -1)
  (page-break-lines-mode +1)
  (setq truncate-lines t)
  (setq whitespace-style nil)
  (setq global-whitespace-mode nil)
  (setq whitespace-mode nil)
  (setq electric-indent-mode -1)
  (setq show-trailing-whitespace nil)

  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist)))


(defun dashboard/next-button ()
  (interactive)
  (ignore-errors (goto-char (next-button (point)))))
(defun dashboard/previous-button ()
  (interactive)
  (ignore-errors (goto-char (previous-button (point)))))

(defun dashboard/first-button ()
  (interactive)
  (goto-char (point-min))
  (dashboard/next-button))

(defun dashboard/last-button ()
  (interactive)
  (goto-char (point-max))
  (dashboard/previous-button)
  (beginning-of-line-text))

(provide 'config-dashboard)
