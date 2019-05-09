;;; -*- lexical-binding: t -*-
;;; config-navigation.el --- Moving around.

;; Copyright (C) 2015 Bodil Stokke

;; Author: Bodil Stokke <bodil@bodil.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Make PgUp/Dn move the point.
(setq scroll-error-top-bottom t)

;; Avy is a quick way to jump around your buffers.
;; https://github.com/abo-abo/avy
(use-package avy
  :commands (avy-goto-char-2 avy-goto-line avy-go-word-or-subword-1 avy-goto-char-timer)
  :bind ("C-." . avy-goto-char)
  :config
  (setq avy-all-windows 'all-frames
        avy-background t
        avy-timeout-seconds 0.3)
  (with-eval-after-load "isearch"

    (define-key isearch-mode-map (kbd "C-;") 'avy-isearch)))

;; Smart home key.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Consider CamelCase chunks as words when navigating.
(global-subword-mode 1)

;; Enhance C-x o when more than two windows are open.
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-swap-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Use C-x M-p to kill the buffer in the other window, revealing
;; the next buffer in the stack.
(global-set-key
 (kbd "C-x M-p")
 (lambda () (interactive)
   (save-excursion
     (other-window 1)
     (quit-window))))

;; Display incremental search stats in the modeline.
(use-package anzu
  :demand t
  :config
  (global-anzu-mode 1)
  (setq anzu-mode-lighter "" anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil :foreground "yellow")
  ;; Anzu provides a version of `query-replace' and friends which give visual
  ;; feedback when composing regexps. Let's replace the regular versions.
  :bind(("C-%" . anzu-query-replace-at-cursor)
        ("M-%" . anzu-query-replace)
        ("C-M-%" . anzu-query-replace-regexp))
  :diminish anzu-mode)

(use-package ace-link
  :demand t
  :config (ace-link-setup-default))

(use-package thingatpt :defer t)



(provide 'config-navigation)
