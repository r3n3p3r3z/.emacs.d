(require 'config-lib)

(defun config-fonts/spec-to-list (spec)
  (s-split "-" spec))

(defun config-fonts/list-to-spec (spec)
  (s-join "-" spec))

(defun config-fonts/update-font-spec-size (spec increment)
  (config-fonts/list-to-spec
   (-update-at 7 (lambda (i) (number-to-string
                              (+ (string-to-number i) increment)))
               (config-fonts/spec-to-list spec))))

(defun config-fonts/update-font-size (increment)
  (set-frame-font
   (config-fonts/update-font-spec-size (frame-parameter nil 'font) increment)))

(global-set-key (kbd "C-M--") (lambda () (interactive)
                                (config-fonts/update-font-size -1)))
(global-set-key (kbd "C-M-=") (lambda () (interactive)
                                (config-fonts/update-font-size 1)))

(provide 'config-fonts)
;;; config-fonts.el ends here
