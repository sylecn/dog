;;; dog.el --- simple music player

;; Copyright (C) 2009  Yuanle Song

;; Author: Yuanle Song <sylecn@gmail.com>
;; Keywords: multimedia

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

;;; Commentary:

;;
;;
;; TODO
;; how to handle window resize? add hook?
;; check all items on README and zTODO.

;;; Code:

(defgroup dog nil
  "dog is a simple music player based on mplayer."
  :tag "dog"
  :version 23.1
  :group 'multimedia)

(defconst dog-version "1.0dev"
  "version number of dog.")

(defun dog-version ()
  "show dog version"
  (interactive)
  (message "dog %s" dog-version))

;;========================
;; customizable variables
;;========================

(defcustom dog-default-music-dir nil
  "where to find music files"
  :type 'string
  :group 'dog)

(defcustom dog-config-dir (concat user-emacs-directory "dog/")
  "dir to store dog play lists and config files."
  :type 'string
  :group 'dog)

(defcustom dog-default-list-name "default"
  "name for default list"
  :type 'string
  :group 'dog)

(defcustom dog-loop-on nil
  "if non-nil loop will be on by default."
  :type 'boolean
  :group 'dog)

(defcustom dog-random-on nil
  "if non-nil random will be on by default."
  :type 'boolean
  :group 'dog)

(defcustom dog-use-pattern-entry 'ask-no
  "when adding a file containing *, should I create a pattern entry (which will automatically update) or just add the files matching the pattern. possible values are `ask-no' (default), `ask-yes', `yes', `no'. setting to nil means no."
  :type 'symbol
  :group 'dog)

(defcustom dog-recursive-add-directory nil
  "when adding a dir, whether or not to include sub dirs.")

;; when adding ~/musics/*, should only add music files, ignore files with no ext
(defcustom dog-music-file-pattern ""
  "a regexp. when adding a dir, only add files matching this pattern. empty means add everything."
  :type 'string
  :group 'dog)

(defcustom dog-music-file-pattern-exclude "\\.\\(?:rar\\|zip\\|tar"
  "a regexp. when adding a dir, exclude files matching this pattern"
  :type 'string
  :group 'dog)

;;===========================
;; across sessions
;;===========================

;; what's under ~/.emacs.d/dog/
;; config.el
;; (dog-version)
;;
;; foo.el bar.el ...
;; see document in dog-list.el before dog-default-list.

(defvar dog-last-active-list nil
  "the name (string) of last active list.")

(defun dog-read-config ()
  "read config from `dog-config-dir'"
  (interactive)
  )

(defun dog-write-config ()
  "write config to `dog-config-dir'"
  (interactive)
  )

;;====================
;; ordinary variables
;;====================

(defvar dog-buffer-list nil
  "contains all opened list buffers")
;;TODO when dog list buffer is killed, update this variable.

;;==============
;; common utils
;;==============

(defun dog-buffer-name (l)
  "return the buffer name for given list name (string)"
  (concat " dog-" l))

(defun dog-list-name (b)
  "return the list name of given buffer name (string)"
  (interactive)
  (substring b 5))

(window-width)

;; (require 'dog-list)
;; (require 'dog-lists)

;;=====
;; dog
;;=====

(defun dog-find-list (l)
  "if l is already opened, show that buffer. else open list l (string)."
  (interactive)
  (let ((bufname (dog-buffer-name l)))
    (if (member bufname dog-buffer-list)
	(switch-to-buffer bufname)
      (get-buffer-create bufname)
      (dog-list-mode))))

;; major interface
(defun dog ()
  "start dog. read configs in `dog-config-dir'. open list in `dog-last-active-list' or `dog-default-list'."
  (interactive)
  (dog-read-config)
  (if dog-last-active-list
      (dog-find-list dog-last-active-list)
    (dog-find-list dog-default-list)))

(provide 'dog)

;;; dog.el ends here
