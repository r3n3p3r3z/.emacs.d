;;; -*- lexical-binding: t -*-
;;; config-flow.el --- JavaScript is slightly less horrid with types.

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

(use-package flow-mode
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-flow 'flow-mode)
    (flycheck-add-mode 'javascript-eslint 'flow-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-flow))
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files ".flowconfig"))
  (setq flow-binary (or (config/resolve-exec "flow") "flow"))
  (add-to-list 'auto-mode-alist '("\\.flowconfig\\'" . conf-mode)))



(provide 'config-flow)
;;; config-flow.el ends here
