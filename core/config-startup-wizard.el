;;; -*- lexical-binding: t -*-
;;; config-startup-wizard.el --- Ask questions and configure on first run.

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

(require 'config-personal-taste)


(defun config-startup-wizard/get-style ()
  (x-popup-dialog
   t '("Light or dark background?"
       ("Light" . light)
       ("Dark" . dark))))


(defun config-startup-wizard/get-training-wheels ()
  (x-popup-dialog
   t '("Would you like to get rid of the toolbar,
menu bar and scrollbar?

This is how adult emacsen roll, but you
might want to get comfortable with being
emacs before you assume your final form.

This is OK too; no pressure yet."
       ("I'm ready" . nil)
       ("Wait, I'm scared" . t))))

(defun config-startup-wizard ()
  (interactive)

  (x-popup-dialog
   t '("Welcome to the Church of Emacs, my child!

This is the first time you've run it, so let's
start off by asking you some basic questions
about how you like to emacs.

If you change your mind about any of these
decisions, you can re-run this wizard with
`M-x config-startup-wizard` (that is Alt+X
config-startup-wizard <enter> to non-native
speakers)."
       ("I am ready to emacs" . t)) t)

  (customize-save-variable
   'config-personal-taste/style
   (config-startup-wizard/get-style))

  (customize-save-variable 'config-personal-taste/run-wizard nil)
  (setq config/wizard-did-run t)
  (config/select-modules))

(when config-personal-taste/run-wizard
  (config-startup-wizard))



(provide 'config-startup-wizard)
