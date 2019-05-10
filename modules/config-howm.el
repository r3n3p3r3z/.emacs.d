(use-package howm
  :after howm
  :config
    (setq howm-view-preview-narrow nil)
  (add-hook 'org-mode-hook 'howm-mode)
  (add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))

  (setq howm-view-split-horizontally t)

  (setq howm-menu-refresh-after-save nil)
  (setq howm-menu-expiry-hours 6)  ;; cache menu N hours
  (setq howm-menu-file "0000-00-00-000000.txt")  ;; don't *search*

  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)

  ;; howm-menu
  (defun howm-menu-with-j1 (orig-fun &rest args)
    (setq howm-view-grep-option "-nH --no-heading -j1 --color never")
    (apply orig-fun args)
    (setq howm-view-grep-option "-nH --no-heading --color never"))

  (advice-add 'howm-menu-refresh :around #'howm-menu-with-j1)

  (setq howm-view-search-in-result-correctly t)  ;; be aware of paragraph

  (setq howm-view-list-title-type 2) ;; Show title before summary.
  (setq howm-view-summary-format "") ;; If you want to delete file names.


  (defun howm-search-title (title)
    (interactive "sSearch title: ")
    (message title)
    (howm-search (format "^* +%s" (regexp-quote title)) nil))

  (defun howm-list-grep-in-new-frame (&optional completion-p)
    (interactive "P")
    (select-frame (make-frame))
    (howm-list-grep completion-p))

  (defvar *howm-new-frame* nil)

  (defun howm-new-frame ()
    (when *howm-new-frame*
      (select-frame (make-frame))))
  (add-hook 'howm-view-before-open-hook 'howm-new-frame)

  (defun howm-open-new-frame (opener)
    ;; move cursor back from contents to summary in the original frame
    (let (new-frame)
      (save-window-excursion
        (let ((*howm-new-frame* t))
          (funcall opener))
        (setq new-frame (selected-frame)))
      (select-frame new-frame)))

  (defun howm-open-new-frame-summary ()
    (interactive)
    (howm-open-new-frame #'howm-view-summary-open-sub))

  (defun howm-open-new-frame-contents ()
    (interactive)
    (howm-open-new-frame #'howm-view-contents-open-sub))


  (defun howm-create-and-link (&optional which-template)
    (interactive "p")
    (let ((b (current-buffer))
          (p (point)))
      (prog1
          (howm-create which-template)
        (let ((f (buffer-file-name)))
          (when (and f (buffer-file-name b))
            (with-current-buffer b
              (goto-char p)
              (insert (format howm-template-file-format
                              (abbreviate-file-name f))
                      "\n")))))))

  (defun howm-open-from-calendar ()
    (interactive)
    (require 'howm-mode)
    (let* ((mdy (calendar-cursor-to-date t))
           (m (car mdy))
           (d (second mdy))
           (y (third mdy))
           (ti (encode-time 0 0 0 d m y))
           (pc (howm-folder-get-page-create howm-directory (howm-file-name ti)))
           (page (car pc))
           (createp (cdr pc)))
      (other-window 1)
      (howm-page-open page)
      (if createp
          (howm-create-here)
        (howm-set-mode))))
  (require 'calendar)

  )

(provide 'config-howm)
