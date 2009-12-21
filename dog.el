;;; dog.el --- dog music player

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
;;
;; write some statistics to config file:
;; dog-total-playing-time
;; dog-total-list-created
;;
;; check all items on README and zTODO before releasing
;;


;;; Code:

;;debug only
;; (add-to-list 'load-path ".")
;; (progn (setq dog-buffer-list nil dog-list-last-used-dir nil dog-file-display-name-alist nil dog-list-glob-remove-alist nil))
;; (progn (load-file "dog-utils.el") (load-file "dog-common.el") (load-file "dog-list.el") (load-file "dog.el"))

(eval-when-compile
  (require 'dog-common)
  (require 'dog-utils)
  (require 'dog-list)
  ;; (require 'dog-lists)
)


;;==================
;; global functions
;;==================

(defun dog-global-next ()
  "play next file in list. loop if possible. stop if there is none."
  (interactive)
  )
(defun dog-global-previous ()
  "play previous file in list. loop if possible. stop if there is none."
  (interactive)
  )
(defun dog-global-repeat-track ()
  "repeat playing current track forever. until you goto next file manually."
  (interactive)
  )
(defun dog-global-toggle-loop ()
  "toggle list loop"
  ;; only valid when playing
  (interactive)
  )
(defun dog-global-show-list ()
  "show active list. i.e. switch to `dog-active-buffer'"
  (interactive)
  (if dog-active-buffer
      (dog-find-list dog-active-buffer)
    (dog-find-list dog-default-list-name)))
(defun dog-global-pause ()
  "pause or continue from pause"
  (interactive)
  )

(global-set-key (kbd "C-c e n") 'dog-global-next)
(global-set-key (kbd "C-c e p") 'dog-global-previous)
(global-set-key (kbd "C-c e r") 'dog-global-repeat-track)
(global-set-key (kbd "C-c e l") 'dog-global-toggle-loop)
(global-set-key (kbd "C-c e e") 'dog-global-show-list)
(global-set-key (kbd "C-c e SPC") 'dog-global-pause)

;;==========
;; dog init
;;==========

(defun dog-version ()
  "show dog version"
  (interactive)
  (message "dog %s" dog-version))

(defun dog-find-list (l)
  "if l is already opened, show that buffer. else open list l (string)."
  (interactive)
  (let ((bufname (dog-buffer-name l)))
    (if (member bufname dog-buffer-list)
	(switch-to-buffer bufname)
      (let ((buf (get-buffer-create bufname)))
	(switch-to-buffer buf)
	(dog-list-mode)))))

;; major interface
(defun dog ()
  "start dog.

read configs in `dog-config-dir'. open list in `dog-last-active-list' or `dog-default-list-name'."
  (interactive)
  (dog-read-config)
  ;; switch to last-active-list. this is different from active-buffer
  (if dog-last-active-list
      (dog-find-list dog-last-active-list)
    (dog-find-list dog-default-list-name)))

(provide 'dog)

;;; dog.el ends here
