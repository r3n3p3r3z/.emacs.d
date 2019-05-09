;;; -*- lexical-binding: t -*-
;;; config-module-index.el --- The index of available Config Emacs modules.

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

(setq custom-safe-themes t)

(setq
 config/available-modules
 '((config-appearance "how Emacs looks" :recommended)
   (config-fonts "adjust font size on the fly" :recommended)
   (config-screencast "create and view interactive screencast" :recommended)
   (config-dashboard "a dashboard for emacs" :recommended)
   (config-help "ways to get more help" :recommended)
   (config-general "basic editor settings" :recommended)
   (config-navigation "moving around better" :recommended)
   (config-ext-window-nav "alternative window navigation" :recommended)
   (config-editing "editing improvements (multiple cursors etc)" :recommended)
   (config-complete "auto completion" :recommended)
   (config-snippets "snippet management" :recommended)
   (config-orgmode "your personal everything manager" :recommended)
   (config-elisp "Emacs Lisp" :recommended)
   (config-helm "advanced selection and narrowing" :recommended)
   (config-dired "enhanced file manager" :recommended)
   (config-writeroom-mode "Minor mode for distraction-free writing" :recommended)
   (config-editing "editing improvements (multiple cursors etc)" :recommended)
   (config-flycheck "run linters automatically with Flycheck" :recommended)
   (config-markdown "Markdown support" :recommended)
   (config-nov "featureful EPUB reader mode" :optional)
   (config-agenda "a template for org-agenda" :optional)
   (config-deft "quickly browse, filter, and edit plain text notes" :optional)
   (config-howm "write wiki-like fragmentary notes, and read collectively" :optional)
   (config-hyperbole "Hypertextual Information Manager" :optional)
   (config-git "Git tools" :optional)
   (config-project "manage projects with Projectile" :optional)
   (config-codestyle "code formatting, whitespace management" :optional)
   (config-pdf-tools "powerful pdf viewer" :optional)
   (config-eshell "the native Emacs shell" :optional)
   (config-refactor "easy access to refactoring tools" :optional)
   (config-emoji "display Unicode emoji even if your system doesn't" :optional)
   (config-lsp "Language Server Protocol" :optional)
   (config-html "HTML, CSS and friends" :optional)
   (config-javascript "JavaScript language support" :optional)
   (config-js-web-mode "alternative JS support using web-mode" :optional)
   (config-experimental "playground for experimenting with new modules" :optional)



))

(require 'cl)
(defcustom config/modules (mapcar #'car
                                (remove-if-not
                                 (lambda (i) (equal :recommended (caddr i)))
                                 config/available-modules))
  "Your choice of Config Emacs modules.")

(defun config/load-modules ()
  (interactive)
  (dolist (module config/modules) (require module nil t))
  (run-hooks 'config/modules-loaded-hook))

(provide 'config-module-index)
;;; config-module-index.el ends here
