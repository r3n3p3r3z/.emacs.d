(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "I'm powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Emacs requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Figure out the current hostname.
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

(eval-and-compile
  (defvar dotfiles-dir (expand-file-name user-emacs-directory)
    "The path to the emacs.d directory.")

  (defvar dotfiles-core-dir (expand-file-name "core" dotfiles-dir)
    "The home of Emacs's core functionality.")

  (defvar dotfiles-personal-dir (expand-file-name "personal" dotfiles-dir)
    "This directory is for your personal configuration.

Users of Emacs Emacs are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Emacs.")

  (defvar dotfiles-modules-dir (expand-file-name  "modules" dotfiles-dir)
    "This directory houses all of the built-in Emacs modules.")

  (defvar dotfiles-modules-file (expand-file-name "modules.el" dotfiles-personal-dir)
    "This file contains a list of modules that will be loaded by Emacs.")

  (defvar dotfiles-personal-preload-dir (expand-file-name "preload" dotfiles-personal-dir)
    "This directory is for your personal configuration, that you want loaded before Emacs.")

  (defvar dotfiles-vendor-dir (expand-file-name ".vendor" dotfiles-dir)
    "This directory houses packages that are not yet available in ELPA (or MELPA).")

  (defvar dotfiles-savefile-dir (expand-file-name ".savefile" dotfiles-dir)
    "This folder stores all the automatically generated save/history-files.")

  (defvar dotfiles-local-dir (concat dotfiles-dir ".local/")
    "Root directory for local Emacs files. Use this as permanent storage for files
   that are safe to share across systems (if this config is symlinked across
   several computers).")

  (defvar dotfiles-host-dir (concat dotfiles-local-dir "@" (system-name))
    "Directory for hostname-specific file storage. Used by `my/etc-dir' and
   `my/cache-dir'.")

  (defvar dotfiles-etc-dir (concat dotfiles-host-dir "/etc/")
    "Host-namespaced directory for non-volatile storage. These are not deleted or
   tampored with by MY/ functions. Use this for dependencies like servers or
   config files that are stable (i.e. it should be unlikely that you need to delete
   them if something goes wrong).")

  (defvar dotfiles-cache-dir (concat dotfiles-host-dir "/cache/")
    "Host-namespaced directory for volatile storage. Deleted when `my/reset' is
   called. Use this for transient files that are generated on the fly like caches
   and temporary files. Anything that may need to be cleared if there are
   problems.")
  )

(defvar dotfiles-custom-dir (concat dotfiles-dir ".custom/")
  "Where custom lisp files are stored")

(defvar dotfiles-screencast-dir (concat dotfiles-dir "screencast/")
  "Where custom lisp files are stored")

(dolist (dir (list dotfiles-local-dir dotfiles-etc-dir dotfiles-cache-dir dotfiles-custom-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))


(setq url-configuration-directory (concat dotfiles-cache-dir "url/"))
(setq tramp-persistency-file-name (concat dotfiles-cache-dir "tramp"))


;; Always load newest byte code
(setq load-prefer-newer t)

(unless (file-exists-p dotfiles-savefile-dir)
  (make-directory dotfiles-savefile-dir))

(defun dotfiles-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (dotfiles-add-subfolders-to-load-path name)))))

(add-to-list 'load-path dotfiles-core-dir)
(add-to-list 'load-path dotfiles-modules-dir)
(add-to-list 'load-path dotfiles-vendor-dir)
(add-to-list 'load-path dotfiles-custom-dir)
(dotfiles-add-subfolders-to-load-path dotfiles-custom-dir)
(dotfiles-add-subfolders-to-load-path dotfiles-vendor-dir)

(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `dotfiles-personal-preload-dir'
(when (file-exists-p dotfiles-personal-preload-dir)
  (message "Loading personal configuration files in %s..." dotfiles-personal-preload-dir)
  (mapc 'load (directory-files dotfiles-personal-preload-dir 't "^[^#\.].*el.*org$")))

(message "Loading Emacs's core...")

(setq custom-file (expand-file-name "custom.el" dotfiles-dir))

(load custom-file 'noerror)


;; Load the modules
(require 'config-package)
(require 'config-lib)
(require 'config-module-index)
(require 'config-module-selector)
(require 'config-update)
(require 'config-personal-taste)
(require 'config-startup-wizard)
(require 'config-set-path)
(require 'config-core)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'config-macos))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'config-linux))


(message "Loading modules...")

;; Load the enabled modules.
(when (not (boundp 'config/wizard-did-run)) (config/load-modules))

;; config changes made through the customize UI will be stored here

;; load the personal settings (this includes `custom-file')
(when (file-exists-p dotfiles-personal-dir)
  (message "Loading personal configuration files in %s..." dotfiles-personal-dir)
  (mapc 'load (delete
               dotfiles-modules-file
               (directory-files dotfiles-personal-dir 't "^[^#\.].*\\.el$"))))

(message "I'm ready to do thy bidding, Master %s!" current-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))



(config-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'config-tip-of-the-day))
