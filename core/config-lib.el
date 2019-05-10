(require 'config-package)

;; Ensure the New Standard Library is installed and always available.
;; f.el    - files and paths  https://github.com/rejeep/f.el
;; s.el    - strings          https://github.com/magnars/s.el
;; dash.el - lists            https://github.com/magnars/dash.el

(use-package f)
(use-package s)
(use-package dash)

(defun core/font-lock-replace-symbol (mode reg sym)
  "Given a major mode `mode', replace the regular expression `reg' with
the symbol `sym' when rendering."
  (font-lock-add-keywords
   mode `((,reg
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,sym 'decompose-region)))))))
(defun core/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (s-trim (shell-command-to-string command)))

(defun core/exec-with-rc (command &rest args)
  "Run a shell command and return a list containing two values: its return
code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun core/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun core/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun core/exec-if-exec (command args)
  "If `command' satisfies `core/is-exec', run it with `args' and return its
output as per `core/exec'. Otherwise, return nil."
  (when (core/is-exec command) (core/exec (s-concat command " " args))))


(defun core/getent (user)
  "Get the /etc/passwd entry for the user `user' as a list of strings,
or nil if there is no such user. Empty fields will be represented as nil,
as opposed to empty strings."
  (-let [ent (core/exec (s-concat "getent passwd " user))]
    (when (not (s-blank? ent))
      (-map (lambda (i) (if (s-blank? i) nil i))
            (s-split ":" ent)))))

(defun core/user-full-name ()
  "Guess the user's full name. Returns nil if no likely name could be found."
  (or (core/exec-if-exec "git" "config --get user.name")
      (elt (core/getent (getenv "USER")) 4)))

(defun core/user-email ()
  "Guess the user's email address. Returns nil if none could be found."
  (or (core/exec-if-exec "git" "config --get user.email")
      (getenv "EMAIL")))



(provide 'config-lib)
;;; config-lib.el ends
