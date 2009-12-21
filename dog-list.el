;;; dog-list.el --- provide dog-list-mode for list buffer

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

;;; Code:

;; a dog list is a list of form
;; (listname loop-p random-p last-played-file FILES)
;;
;; listname is the list name (string)
;; loop-p and random-p can be one of the following
;;
;; nil    use global default settings. see `dog-loop-on' and `dog-random-on'
;; 'yes   turn on the feature
;; 'no    turn off the feature
;;
;; FILES is a list, each element in the list can be one of the following
;;
;; ('static . dir . FILENAMES)
;; ('glob . pattern . EXCLUDE-FILENAMES)
;; ('regexp . pattern . EXCLUDE-FILENAMES)
;;
;; for the 'static type:
;; dir is a path to a dir (string)
;; FILENAMES is a list of filenames (string, without path) in the list
;; (file1 file2 file3 ...)
;;
;; for the 'glob type:
;; pattern is a glob pattern. for example, "~/musics/*.mp3".
;; EXCLUDE-FILENAMES is a list of filenames (string, without path) excluded from
;; the list.
;; (excl-file1 excl-file2 excl-file3 ...)
;;
;; the 'regexp type is reserved, but not implemented.

(defvar dog-default-list
  (list dog-default-list-name t nil nil nil))



;;===========================
;; begin defining major mode
;;===========================

(define-derived-mode dog-list-mode
  fundamental-mode "dog list"
  "Major mode to view, modify and play dog list.
\\{dog-list-mode-map}
Turning on dog-list mode runs the normal hook `dog-list-mode-hook'."
  :syntax-table nil
  :abbrev-table nil
  :group 'dog

  ;; now all we have is buffer name. we will build everything from here.
  (set (make-local-variable 'dog-list-name) (dog-list-name (buffer-name)))

  (make-local-variable 'dog-list)
  (load-file (dog-list-file dog-list-name))
  ;; now dog-list is a valid dog list object

  ;; ;; font lock mode
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      (list 'll-font-lock-keywords t))

  ;; add key bindings
  (define-key dog-list-mode-map (kbd "C-c C-n")
    'll-next-section)
  (define-key dog-list-mode-map [remap indent-new-comment-line]
    'll-indent-new-comment-line)

  ;; post processing
  (set (make-local-variable 'buffer-read-only) t)
  (add-to-list 'dog-buffer-list (buffer-name))
  )

(provide 'dog-list)

;;; dog-list.el ends here
