;;; -*- lexical-binding: t -*-
;;; config-personal-taste.el --- Config Emacs preferences.

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

(defgroup config-emacs nil
  "Your personal taste in Config Emacs."
  :prefix "config-personal-taste/")

(defcustom config-personal-taste/run-wizard t
  "Should we run the Config Emacs startup wizard on the next startup?"
  :group 'config-emacs
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom config-personal-taste/window-state 'maximised
  "Should Emacs maximise its frame on startup, or leave it alone?"
  :group 'config-emacs
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Maximise" maximised)))

(defcustom config-personal-taste/style 'dark
  "Light or dark colour scheme?"
  :group 'config-emacs
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark" dark)))

(defcustom config-personal-taste/training-wheels t
  "Would you prefer an Emacs experience without the clutter of the menu bar,
toolbar and scrollbar?"
  :group 'config-emacs
  :type '(choice (const :tag "Yes, please" t)
                 (const :tag "I'm not ready for that" nil)))



(provide 'config-personal-taste)
;;; config-personal-taste.el ends here
