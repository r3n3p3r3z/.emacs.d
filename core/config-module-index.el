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
   (config-dashboard "a dashboard for emacs" :recommended)
   (config-general "basic editor settings" :recommended)
   (config-navigation "moving around better" :recommended)
   (config-complete "auto completion" :recommended)
   (config-snippets "snippet management" :recommended)
   (config-orgmode "your personal everything manager" :recommended)
   (config-help "ways to get more help" :recommended)
   (config-elisp "Emacs Lisp" :recommended)
   (config-helm "advanced selection and narrowing" :recommended)
   (config-dired "enhanced file manager" :recommended)
   (config-writeroom-mode "Minor mode for distraction-free writing" :recommended)
   (config-nov "featureful EPUB reader mode" :recommended)
   (config-deft "quickly browse, filter, and edit plain text notes" :optional)
   (config-howm "write wiki-like fragmentary notes, and read collectively" :optional)
   (config-hyperbole "Hypertextual Information Manager" :optional)
   (config-git "Git tools" :optional)
   (config-flycheck "run linters automatically with Flycheck" :optional)
   (config-project "manage projects with Projectile" :optional)
   (config-codestyle "code formatting, whitespace management" :optional)
   (config-editing "editing improvements (multiple cursors etc)" :optional)
   (config-ido "improved file selector etc" :optional)
   (config-pdf-tools "powerful pdf viewer" :optional)
   (config-eshell "the native Emacs shell" :optional)
   (config-unicode "enhanced Unicode support (warning: slows startup)" :optional)
   (config-refactor "easy access to refactoring tools" :optional)
   (config-emoji "display Unicode emoji even if your system doesn't" :optional)
   (config-smart-mode-line "cleaner, lighter modeline (no Nyan Cat)" :optional)
   (config-ext-window-nav "alternative window navigation" :recomended)
   (config-lsp "Language Server Protocol" :optional)
   (config-html "HTML, CSS and friends" :optional)
   (config-markdown "Markdown support" :optional)
   (config-javascript "JavaScript language support" :optional)
   (config-js-web-mode "alternative JS support using web-mode" :optional)
   (config-flow "the Flow type checker for JS" :optional)
   (config-purescript "PureScript language support" :optional)
   (config-erlang "Erlang language support" :optional)
   (config-elixir "Elixir language support" :optional)
   (config-haskell "Haskell language support" :optional)
   (config-rust "Rust language support" :optional)))

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
