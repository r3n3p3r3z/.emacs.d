;;; -*- lexical-binding: t -*-
;;; config-git.el --- Things for working with Git.

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

(require 'config-package)

;; Invoke Magit by typing C-x g, and you can thank me later.
;; See http://magit.github.io/ for instructions.
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

;; Use M-x gist-buffer or M-x gist-region to create a gist
;; directly from the current buffer or selection.
(use-package gist
  :commands (gist-list gist-buffer gist-region gist-buffer-private gist-region-private))

(use-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(use-package gitignore-mode
  :mode "/\\.gitignore$")



;; Mark uncommitted changes in the fringe.
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(use-package git-link
  :commands (git-link git-link-commit git-link-homepage))

(defvar git-link-open-in-browser)

(defun my/git-browse ()
  "Open the website for the current version controlled file. Fallback to
repository root."
  (interactive)
  (cl-destructuring-bind (beg end)
      (if buffer-file-name (git-link--get-region))
    (let ((git-link-open-in-browser t))
      (git-link (git-link--select-remote) beg end))))

(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame))



(provide 'config-git)
