  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)


(defvar dotfiles--file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)


(defvar init-el-start-time (current-time) "Time when init.el was started")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "I'm powering up... Be patient, Master %s!" current-user)

(defvar dotfiles-personal-dir (expand-file-name ".personal.d/" "~/")
  "This directory is for your personal configuration.")

(defvar dotfiles-custom-dir (concat user-emacs-directory "lisp/")
  "Where custom lisp files are stored")

(defvar dotfiles-local-dir (concat user-emacs-directory ".local/")
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

(dolist (dir (list dotfiles-custom-dir dotfiles-personal-dir dotfiles-local-dir dotfiles-host-dir dotfiles-cache-dir dotfiles-etc-dir))
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

;; Skip the default splash screen.
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Figure out the current hostname.
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))

;; Define where we want to keep `loaddefs.el` (our autoload declarations) and
;; `custom.el` (our user settings file).
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Load the user settings from `custom.el`.
(load custom-file 'noerror)

(message "Loading modules...")


;; set paths to manually installed Org-mode (from git; instead of built-in Org-mode)
(add-to-list 'load-path "~/.emacs.d/contrib/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/contrib/org-mode/contrib/lisp" t)
(setq custom-safe-themes t)
(require 'org)

(defvar current-date-time-format "%a %b %d %Y-%m-%dT%H:%M:%S "
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun tangle-config-org ()
  "This function will write all source blocks from =config.org= into =config.el= that are ...

- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat user-emacs-directory "config.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat user-emacs-directory "config.org")
	  (let* (
		 (org_block_info (org-babel-get-src-block-info 'light))
		 ;;(block_name (nth 4 org_block_info))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword)
		 )
	    (save-excursion
	      (catch 'exit
		;;(when (string= "" block_name)
		;;  (message "Going to write block name: " block_name)
		;;  (add-to-list 'body-list (concat "message(\"" block_name "\")"));; adding a debug statement for named blocks
		;;  )
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))
		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
		  (setq match_for_TODO_keyword (match-string 1)))))
	    (unless (or (string= "no" tfile)
			(string= "DISABLED" match_for_TODO_keyword)
			(not (string= "emacs-lisp" lang)))
	      (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
					      "(message \"config • " (org-get-heading) " …\")\n\n")
			   )
	      (add-to-list 'body-list body)
	      ))))
      (with-temp-file output-file
        (insert ";; ============================================================\n")
        (insert ";; Don't edit this file, edit config.org' instead ...\n")
        (insert ";; Auto-generated at " (format-time-string current-date-time-format (current-time)) " on host " system-name "\n")
        (insert ";; ============================================================\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "—————• Wrote %s" output-file))))


(let ((orgfile (concat user-emacs-directory "config.org"))
      (elfile (concat user-emacs-directory "config.el"))
      (gc-cons-threshold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (tangle-config-org))
  (load-file elfile))

;; when config.org is saved, re-generate config.el:
(defun tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
	(let ((orgfile (concat user-emacs-directory "config.org"))
		  (elfile (concat user-emacs-directory "config.el")))
	  (tangle-config-org))))
(add-hook 'after-save-hook 'tangle-config-org-hook-func)

(defun tangle-personal-config ()
(interactive)
  "This function will write all source blocks from =personal-config.org= into =personal-config.el= that are ...
- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat "~/.personal.d/" "personal-config.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat "~/.personal.d/" "personal-config.org")
	  (let* (
		 (org_block_info (org-babel-get-src-block-info 'light))
		 ;;(block_name (nth 4 org_block_info))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword)
		 )
	    (save-excursion
	      (catch 'exit
		;;(when (string= "" block_name)
		;;  (message "Going to write block name: " block_name)
		;;  (add-to-list 'body-list (concat "message(\"" block_name "\")"));; adding a debug statement for named blocks
		;;  )
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))
		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
		  (setq match_for_TODO_keyword (match-string 1)))))
	    (unless (or (string= "no" tfile)
			(string= "DISABLED" match_for_TODO_keyword)
			(not (string= "emacs-lisp" lang)))
	      (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
					      "(message \"config • " (org-get-heading) " …\")\n\n")
			   )
	      (add-to-list 'body-list body)
	      ))))
      (with-temp-file output-file
        (insert ";; ============================================================\n")
        (insert ";; Don't edit this file, edit personal-config.org' instead ...\n")
        (insert ";; Auto-generated at " (format-time-string current-date-time-format (current-time)) " on host " system-name "\n")
        (insert ";; ============================================================\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "—————• Wrote %s" output-file))))


(defun tangle-personal-config-hook-func ()
  (when (string= "personal-config.org" (buffer-name))
	(let ((orgfile (concat    dotfiles-personal-dir "personal-config.org"))
		  (pfile (concat dotfiles-personal-dir "personal-config.el")))
	  (tangle-personal-config))))

(add-hook 'after-save-hook 'tangle-personal-config-hook-func)


      (let ((orgfile (concat dotfiles-personal-dir "personal-config.org"))
            (pfile   (concat dotfiles-personal-dir "personal-config.el"))
      (gc-cons-threshold most-positive-fixnum))

        (when (or (not (file-exists-p pfile))
            (file-newer-than-file-p orgfile pfile))
    (tangle-personal-config))
  (load-file pfile))

(run-hooks 'my/post-init-hook)

(add-hook! 'emacs-startup-hook
  (setq file-name-handler-alist dotfiles--file-name-handler-alist))


  (add-hook! 'emacs-startup-hook
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1))

(message "→★ loading init.el in %.2fs" (float-time (time-subtract (current-time) init-el-start-time)))
(message "I'm ready to do thy bidding, Master %s!" current-user)
