
(use-package helm
  :config
  (require 'helm-config)
  (require 'helm)
  ;; Activate Helm.
  (helm-mode 1)
  (with-eval-after-load "config-project"
    (use-package helm-projectile
      ;; A binding for using Helm to pick files using Projectile,
      ;; and override the normal grep with a Projectile based grep.
      :bind (("C-c C-f" . helm-projectile-find-file-dwim)
             ("C-x C-g" . helm-projectile-grep))
      :config (helm-projectile-on)))
(setq helm-candidate-number-limit 50
          ;; Remove extraineous helm UI elements
          helm-mode-line-string nil
          helm-ff-auto-update-initial-value nil
          helm-find-files-doc-header nil
          helm-imenu-execute-action-at-once-if-one nil
          ;; disable special behavior for left/right, M-left/right keys.
          helm-ff-lynx-style-map nil)


      (setq helm-M-x-fuzzy-match 'fuzzy
          helm-ag-fuzzy-match 'fuzzy
          helm-apropos-fuzzy-match 'fuzzy
          helm-apropos-fuzzy-match 'fuzzy
          helm-bookmark-show-location 'fuzzy
          helm-buffers-fuzzy-matching 'fuzzy
          helm-completion-in-region-fuzzy-match 'fuzzy
          helm-completion-in-region-fuzzy-match 'fuzzy
          helm-ff-fuzzy-matching 'fuzzy
          helm-file-cache-fuzzy-match 'fuzzy
          helm-flx-for-helm-locate 'fuzzy
          helm-imenu-fuzzy-match 'fuzzy
          helm-lisp-fuzzy-completion 'fuzzy
          helm-locate-fuzzy-match 'fuzzy
          helm-mode-fuzzy-match 'fuzzy
          helm-projectile-fuzzy-match 'fuzzy
          helm-recentf-fuzzy-match 'fuzzy
          helm-semantic-fuzzy-match 'fuzzy)
  ;; Tell Helm to resize the selector as needed.
  (helm-autoresize-mode 1)
  ;; Make Helm look nice.
  (setq-default helm-display-header-line nil
                helm-autoresize-min-height 10
                helm-autoresize-max-height 35
                helm-split-window-in-side-p t

                helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t)
  (set-face-attribute 'helm-source-header nil :height 0.75)
  ;; Replace common selectors with Helm versions.
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-g" . helm-do-grep)
         ("C-x b" . helm-buffers-list)
         ("C-x c g" . helm-google-suggest)
         ("C-t" . helm-imenu)
         ("M-y" . helm-show-kill-ring)))

;; Enrich isearch with Helm using the `C-S-s' binding.
;; swiper-helm behaves subtly different from isearch, so let's not
;; override the default binding.
(use-package swiper-helm
  :bind (("C-S-s" . swiper-helm)))

;; Enable fuzzy matching in Helm navigation.
(use-package helm-flx
  :config
  (with-eval-after-load "helm"
    (require 'helm-flx)
    (helm-flx-mode 1)))

;; Set up a couple of tweaks from helm-ext.
(use-package helm-ext
  :config
  (helm-ext-ff-enable-skipping-dots t)
  (helm-ext-ff-enable-auto-path-expansion t))

;; Use Helm to complete with multiple matches in eshell.
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)))



;; Bind C-c C-e to open a Helm selection of the files in your .emacs.d.
;; We get the whole list of files and filter it through `git check-ignore'
;; to get rid of transient files.
(defun config-helm/gitignore (root files success error)
  (let ((default-directory root))
    (let ((proc (start-process "gitignore" (generate-new-buffer-name "*gitignore*")
                               "git" "check-ignore" "--stdin"))
          (s (lambda (proc event)
               (if (equal "finished\n" event)
                   (funcall success
                            (with-current-buffer (process-buffer proc)
                              (s-split "\n" (s-trim (buffer-string)))))
                 (funcall error event))
               (kill-buffer (process-buffer proc))
               (delete-process proc))))
      (set-process-sentinel proc s)
      (process-send-string proc (concat (s-join "\n" files) "\n"))
      (process-send-eof proc))))

(defun config-helm/files-in-repo (path success error)
  (let ((files (f-files path nil t)))
    (config-helm/gitignore path files
                         (lambda (ignored)
                           (funcall success (-difference files ignored)))
                         error)))

(defun config-helm/find-files-in-emacs-d ()
  (interactive)
  (config-helm/files-in-repo
   dotfiles-dir
   (lambda (files)
     (let ((relfiles (-filter
                      (lambda (f) (not (f-descendant-of? f ".git")))
                      (-map (lambda (f) (f-relative f dotfiles-dir)) files))))
       (find-file
        (concat dotfiles-dir
                (helm :sources (helm-build-sync-source ".emacs.d" :candidates relfiles)
                      :ff-transformer-show-only-basename helm-ff-transformer-show-only-basename
                      :buffer "*helm emacs.d*")))))
   (lambda (err) (warn "config-helm/find-files-in-emacs-d: %s" err))))

(global-set-key (kbd "C-c C-e") 'config-helm/find-files-in-emacs-d)



(provide 'config-helm)
;;; config-helm.el ends here
