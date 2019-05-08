;;; -*- lexical-binding: t -*-
;;; config-ext-window-nav.el --- Alternative window navigation.

;; Copyright (C) 2016 Bodil Stokke

;; Author: Bodil Stokke <bodil.stokke@tradingtechnologies.com>

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

;; Make C-x o always move to the previous window.
;; C-x C-o will force ace-select window.
;; C-x M-o is ace-swap-window.
(defun config/previous-window ()
  (interactive)
  (-let [current (selected-window)]
    (cond
     ((eq config/--last-window current)
      (ace-select-window))

     ((window-live-p config/--last-window)
      (select-window config/--last-window))

     (t
      (ace-select-window)))
    (setq config/--last-window current)))

(defun config/select-window ()
  (interactive)
  (setq config/--last-window (selected-window))
  (ace-select-window))

(setq config/--last-window (selected-window))

(global-set-key (kbd "C-x o") 'config/previous-window)
(global-set-key (kbd "C-x C-o") 'config/select-window)
(global-set-key (kbd "C-x M-o") 'ace-swap-window)



(provide 'config-ext-window-nav)
;;; config-ext-window-nav.el ends here
