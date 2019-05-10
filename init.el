(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "I'm powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Emacs requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))


  (defvar dotfiles-core-dir (expand-file-name "core" user-emacs-directory)
    "The home of Emacs's core functionality.")
  (defvar dotfiles-modules-dir (expand-file-name  "modules" user-emacs-directory)
    "This directory houses all of the built-in Emacs modules.")
  (defvar dotfiles-personal-dir (expand-file-name "personal" user-emacs-directory)
    "This directory is for your personal configuration.")
  (defvar dotfiles-personal-preload-dir (expand-file-name "preload" dotfiles-personal-dir)
    "This directory is for your personal configuration, that you want loaded before Emacs.")
  (defvar dotfiles-savefile-dir (expand-file-name ".savefile" user-emacs-directory)
    "This folder stores all the automatically generated save/history-files.")
  (defvar dotfiles-custom-dir (concat user-emacs-directory "lisp/")
  "Where custom lisp files are stored")
  (defvar dotfiles-screencasts-dir (expand-file-name "screencasts" user-emacs-directory)
    "The home of Emacs's core functionality.")

(dolist (dir (list dotfiles-custom-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun dotfiles-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (dotfiles-add-subfolders-to-load-path name)))))

;; Always load newest byte code
(setq load-prefer-newer t)

(unless (file-exists-p dotfiles-savefile-dir)
  (make-directory dotfiles-savefile-dir))

(add-to-list 'load-path dotfiles-core-dir)
(add-to-list 'load-path dotfiles-modules-dir)
(add-to-list 'load-path dotfiles-custom-dir)
(add-to-list 'load-path dotfiles-personal-dir)
(dotfiles-add-subfolders-to-load-path dotfiles-custom-dir)

(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `dotfiles-personal-preload-dir'
(when (file-exists-p dotfiles-personal-preload-dir)
  (message "Loading personal configuration files in %s..." dotfiles-personal-preload-dir)
  (mapc 'load (directory-files dotfiles-personal-preload-dir 't "^[^#\.].*el$")))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")


(message "Loading modules...")

;(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq byte-compile-warnings nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Load the modules
(require 'config-lib)
(require 'config-package)
(require 'config-module-index)
(require 'config-module-selector)
(require 'config-update)
(require 'config-personal-taste)
(require 'config-startup-wizard)
(require 'config-set-path)
(require 'config-core)
(require 'config-keybindings)
(require 'config-screencast)




; macOS specific settings
(when (eq system-type 'darwin)
 (require 'config-macos))
 ;Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'config-linux))



;; Load the enabled modules.
(when (not (boundp 'core-wizard-did-run)) (core-load-modules))


;(when (file-exists-p dotfiles-personal-dir)
;  (message "Loading personal configuration files in %s..." dotfiles-personal-dir)
;  (mapc 'load (directory-files dotfiles-personal-dir 't "^[^#\.].*\\.el$")))


(cl-loop for file in (reverse (directory-files-recursively dotfiles-screencasts-dir "\\.el$"))
         do (load file))

(message "I'm ready to do thy bidding, Master %s!" current-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))

(core/eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'core/tip-of-the-day))
