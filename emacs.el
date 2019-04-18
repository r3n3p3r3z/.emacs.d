(setq user-full-name "Your Name Here"
      user-mail-address "user@email.com")

(setq make-backup-files nil)
     (setq backup-directory-alist `((".*" . "~/.saves")))

     (setq backup-by-copying t)
     (setq delete-old-versions t
       kept-new-versions 6
       kept-old-versions 2
       version-control t)


     (setq sentence-end-double-space nil)
     (setq fill-column (1- (window-width)))

     (setq scroll-conservatively 101
           ispell-program-name "aspell")

     (set-language-environment "UTF-8")
     (set-default-coding-systems 'utf-8)
     (setq-default indent-tabs-mode nil)
     (global-auto-revert-mode t)
     (global-hl-line-mode 1)
     (menu-bar-mode -1)
     (scroll-bar-mode -1)
     (tool-bar-mode -1)
     (blink-cursor-mode 0)
     (winner-mode 1)
     (visual-line-mode 1)
     (put 'narrow-to-region 'disabled nil)
     (add-hook 'text-mode-hook 'auto-fill-mode)




     (defalias 'yes-or-no-p 'y-or-n-p) ; remplace yes no par y n

     (setq custom-safe-themes t)

     (setq save-interprogram-paste-before-kill t)

     (save-place-mode)    ; save cursor position between sessions

     (setq help-window-select t)                  ; focus help window when opene

     (defun switch-to-scratch-buffer ()
       "Switch to the `*scratch*' buffer. Create it first if needed."
       (interactive)
       (let ((exists (get-buffer "*scratch*")))
         (switch-to-buffer (get-buffer-create "*scratch*"))
         (when (and (not exists)
                    (not (eq major-mode 'org-mode))
                    (fboundp 'org-mode))
           (funcall 'org-mode))))

     (setq ad-redefinition-action 'accept)

(setq initial-scratch-message "")
(setq initial-buffer-choice t)
(setq initial-major-mode 'org-mode)

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t)))

(setq use-package-always-defer t
      use-package-verbose t)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(eval-when-compile
  (require 'package)

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package which-key
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.3
        which-key-side-window-max-width 0.5
        which-key-idle-delay 0.0
        which-key-min-display-lines 8)
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

(use-package general
  :config
  (general-override-mode 1))

(general-define-key
 :keymaps 'key-translation-map
 "ESC" (kbd "C-g"))


(general-create-definer tyrant-def
  :states '(normal visual insert motion emacs)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")


(general-create-definer despot-def
  :states '(normal insert)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")


(general-def
  "C-x x" 'eval-defun)

(tyrant-def

  ""     nil
  "c"   (general-simulate-key "C-c")
  "h"   (general-simulate-key "C-h")
  "u"   (general-simulate-key "C-u")
  "x"   (general-simulate-key "C-x")

  "TAB"   'switch-to-previous-buffer

   ;; Theme operations
   "t"   '(:ignore t :which-key "themes")

   ;; Quit operations
   "q"	  '(:ignore t :which-key "quit emacs")
   "qq"  'kill-emacs
   "qz"  'delete-frame


   ;; Buffer operations
   "b"   '(:ignore t :which-key "buffer")
   "bb"  'mode-line-other-buffer
   "bd"  'kill-this-buffer
   "b]"  'next-buffer
   "b["  'previous-buffer
   "bq"  'kill-buffer-and-window
   "bR"  'rename-file-and-buffer
   "br"  'revert-buffer

   ;; Window operations
   "w"   '(:ignore t :which-key "window")
   "wm"  'maximize-window
   "w/"  'split-window-horizontally
   "wv"  'split-window-vertically
   "wm"  'maximize-window
   "wu"  'winner-undo
   "ww"  'other-window
   "wd"  'delete-window
   "wD"  'delete-other-windows

   ;; File operations
   "f"   '(:ignore t :which-key "files")
   "fc"  'write-file
   "fe"  '(:ignore t :which-key "emacs")
   "fj"  'dired-jump
   "fl"  'find-file-literally
   "fR"  'rename-file-and-buffer
   "fs"  'save-buffer

   ;; Applications
   "a"   '(:ignore t :which-key "applications")
   "ad"  'dired
   ":"   'shell-command
   ";"   'eval-expression
   "ac"  'calendar
   "oa"  'org-agenda)


      (tyrant-def
       "SPC" 'helm-M-x
       "bm"  'helm-mini
       "ff"  'helm-find-files
       "fr"  'helm-recentf
       "fL"  'helm-locate)

      (tyrant-def
       "p"   '(:ignore t :which-key "projectile")
       "pd"  'helm-projectile-dired-find-dir
       "po"  'helm-projectile-find-other-file
       "pf"  'helm-projectile-find-file
       "fp"  'helm-projectile-find-file
       "pb"  'helm-projectile-switch-to-buffer
       "bp"  'helm-projectile-switch-to-buffer)

(use-package helm
    :hook (after-init . helm-autoresize-mode)
    :config (require 'helm-config)
    :commands (helm-mini
               helm-find-files
               helm-recentf
               helm-locate
               helm-M-x
               helm-flyspell-correct))

  (use-package helm-flyspell
    :commands (helm-flyspell-correct))

  (use-package helm-projectile
    :after (projectile helm))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(use-package helm-swoop
  :ensure t
  :init
  (progn
    (setq helm-swoop-speed-or-color t)
    (global-set-key (kbd "C-s") 'helm-swoop)
    ))

(use-package projectile
  :demand t)

(use-package org
  :ensure org-plus-contrib
  :pin org)

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(use-package hydra
  :ensure t
  :defer 0.5
  :config

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^        ^Visit entry^              ^Date^             ^Other^
^-----^------  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in        _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out       _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel    _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump      _o_   link                 _+_  do later      ^^
_cp_ pomodoro                    
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"

  ("<up>" org-agenda-previous-line)
  ("<down>" org-agenda-next-line)
  ("k" org-agenda-previous-line)
  ("j" org-agenda-next-line)
  ("w" (other-window 1))
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ("cp" org-pomodoro)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))


  (defhydra hydra-registers (:color blue
                                    :hint nil)
    "
_a_: append     _c_: copy-to    _j_: jump       _r_: rectangle-copy   _q_: quit
_i_: insert     _n_: number-to  _f_: frameset   _w_: window-config
_+_: increment  _p_: point-to
"
    ("a" append-to-register)
    ("c" copy-to-register)
    ("i" insert-register)
    ("f" frameset-to-register)
    ("j" jump-to-register)
    ("n" number-to-register)
    ("r" copy-rectangle-to-register)
    ("w" window-configuration-to-register)
    ("+" increment-register)
    ("p" point-to-register)
    ("q" nil :color blue))


  (defhydra hydra-clock (:color blue)
    "
^
^Clock^             ^Do^
^─────^─────────────^──^─────────
_q_ quit            _C_ cancel
^^                  _d_ default task
^^                  _I_ recent task
^^                  _i_ in
^^                  _o_ out
^^                  _j_ jump-current
^^                  _e_ effort
^^                  _r_ report
^^                  ^^
"
    ("q" nil)
    ("C" org-clock-cancel)
    ("d" bh/clock-in-organization-task-as-default)
    ("I" my/org-clock-in)
    ("i" org-clock-in)
    ("o" org-clock-out)
    ("j" org-clock-goto)
    ("r" org-clock-report)
    ("e" org-clock-modify-effort-estimate)
    )

  (defhydra hydra-yasnippet (:color blue)
    "
^
^YASnippet^          ^Do^
^─────────^──────────^──^────────
_q_ quit             _i_ insert
^^                   _m_ mode
^^                   _n_ new
^^                   ^^
"
    ("q" nil)
    ("i" ivy-yasnippet)
    ("m" yas-minor-mode)
    ("n" yas-new-snippet))

  (defhydra hydra-windows (:color pink)
    "
^
^Windows^           ^Window^            ^Zoom^
^───────^───────────^──────^────────────^────^──────
_q_ quit            _b_ balance         _-_ out
^^                  _i_ heighten        _+_ in
^^                  _j_ narrow          _=_ reset
^^                  _k_ lower           ^^
^^                  _l_ widen           ^^
^^                  _s_ swap            ^^
^^                  ^^                  ^^
"
    ("q" nil)
    ("b" balance-windows)
    ("i" enlarge-window)
    ("j" shrink-window-horizontally)
    ("k" shrink-window)
    ("l" enlarge-window-horizontally)
    ("s" switch-window-then-swap-buffer :color blue)
    ("-" text-scale-decrease)
    ("+" text-scale-increase)
    ("=" (text-scale-increase 0)))


  (defhydra hydra-projectile (:hint nil)
    "
^
^Projectile^        ^Buffers^           ^Find^              ^Search^
^──────────^────────^───────^───────────^────^──────────────^──────^────────────
_q_ quit            _b_ list            _d_ directory       _r_ replace
_i_ reset cache     _K_ kill all        _D_ root            _R_ regexp replace
^^                  _S_ save all        _f_ file            _s_ ag
^^                  ^^                  _p_ project         ^^
^^                  ^^                  ^^                  ^^
"
    ("q" nil)
    ("b" counsel-projectile-switch-to-buffer)
    ("d" counsel-projectile-find-dir)
    ("D" projectile-dired)
    ("f" counsel-projectile-find-file)
    ("i" projectile-invalidate-cache :color red)
    ("K" projectile-kill-buffers)
    ("p" counsel-projectile-switch-project)
    ("r" projectile-replace)
    ("R" projectile-replace-regexp)
    ("s" counsel-projectile-ag)
    ("S" projectile-save-project-buffers))

  (defhydra hydra-ag (:color blue :hint nil)
    "
Silver Searcher:     _q_uit
_a_g          _p_roject    _d_ired
_f_iles      file_s_       re_g_exp
_r_egexp     rege_x_p
"
    ("a" ag)
    ("f" ag-files)
    ("r" ag-regexp)
    ("p" ag-project)
    ("s" ag-project-files)
    ("x" ag-project-regexp)
    ("d" ag-dired)
    ("g" ag-dired-regexp)
    ("q" nil)))

(use-package ranger
  :ensure t
  :config
(setq ranger-override-dired-mode t)
(setq ranger-cleanup-on-disable t)
(setq ranger-show-hidden nil))

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-set-initial-state 'deft-mode 'emacs)
  (evil-set-initial-state 'notdeft-mode 'emacs)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)

  (add-hook 'org-capture-mode-hook 'evil-emacs-state)


  (define-key evil-normal-state-map (kbd "q") nil)

  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)

  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

  (define-key evil-normal-state-map (kbd "RET") 'narrow-or-widen-dwim)


  (setq doc-view-continuous t))

(use-package evil-org
  :after evil)

(use-package telephone-line
  :config
  (telephone-line-defsegment my-vc-info ()
    (when vc-mode
      (cond
       ((string-match "Git[:-]" vc-mode)
        (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
          (concat "" (format " %s" branch))))
       ((string-match "SVN-" vc-mode)
        (let ((revision (cadr (split-string vc-mode "-"))))
          (concat "" (format "SVN-%s" revision))))
       (t (format "%s" vc-mode)))))

  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (my-vc-info
                     telephone-line-process-segment))
          (nil    . (telephone-line-buffer-segment
                     telephone-line-projectile-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-flycheck-segment
                     telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil    . (telephone-line-airline-position-segment))))

  (setq display-time-format "%l:%M %p")

  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-mail-file t)

  (display-time-mode t)
  (telephone-line-mode 1))

(use-package yasnippet
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :general
  (tyrant-def
   "y"   '(:ignore t :which-key "yasnippet")
   "yi"  'yas-insert-snippet
   "yv"  'yas-visit-snippet-file
   "yn"  'yas-new-snippet))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package all-the-icons)

(use-package doom-themes)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(doom-themes-org-config)

(use-package org-journal 
 :ensure t 
 :defer t
 :custom
 (org-journal-file-format "%Y.%m.%d.org"))

(define-key org-mode-map (kbd "C-c C-j") nil)

(load-theme 'manoj-dark t)

(defun switch-to-previous-buffer ()
(interactive)
(switch-to-buffer (other-buffer)))

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
