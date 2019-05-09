(require 'config-package)

;; Ensure the New Standard Library is installed and always available.
;; f.el    - files and paths  https://github.com/rejeep/f.el
;; s.el    - strings          https://github.com/magnars/s.el
;; dash.el - lists            https://github.com/magnars/dash.el
(use-package f)
(use-package s)
(use-package dash)



(defun config/font-lock-replace-symbol (mode reg sym)
  "Given a major mode `mode', replace the regular expression `reg' with
the symbol `sym' when rendering."
  (font-lock-add-keywords
   mode `((,reg
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,sym 'decompose-region)))))))



(defun config/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (s-trim (shell-command-to-string command)))

(defun config/exec-with-rc (command &rest args)
  "Run a shell command and return a list containing two values: its return
code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun config/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun config/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun config/exec-if-exec (command args)
  "If `command' satisfies `config/is-exec', run it with `args' and return its
output as per `config/exec'. Otherwise, return nil."
  (when (config/is-exec command) (config/exec (s-concat command " " args))))



(defun config/getent (user)
  "Get the /etc/passwd entry for the user `user' as a list of strings,
or nil if there is no such user. Empty fields will be represented as nil,
as opposed to empty strings."
  (-let [ent (config/exec (s-concat "getent passwd " user))]
    (when (not (s-blank? ent))
      (-map (lambda (i) (if (s-blank? i) nil i))
            (s-split ":" ent)))))

(defun config/user-full-name ()
  "Guess the user's full name. Returns nil if no likely name could be found."
  (or (config/exec-if-exec "git" "config --get user.name")
      (elt (config/getent (getenv "USER")) 4)))

(defun config/user-email ()
  "Guess the user's email address. Returns nil if none could be found."
  (or (config/exec-if-exec "git" "config --get user.email")
      (getenv "EMAIL")))



(provide 'config-lib)
;;; config-lib.el ends here
