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
;; (listname loop-p random-p last-played-file ENTRIES)
;;
;; listname is the list name (string)
;; loop-p and random-p can be one of the following
;;
;; nil    use global default settings. see `dog-loop-on' and `dog-random-on'
;; 'yes   turn on the feature
;; 'no    turn off the feature
;;
;; last-played-file is a pair
;; (count . filename)
;; count is a number means the nth entry in FILES
;; filename is the last played filename
;;
;; ENTRIES is a list, each element in the list can be one of the following
;;
;; ('static . (dir . FILENAMES))
;; ('glob . (dir . (pattern . EXCLUDE-FILENAMES)))
;; ('regexp . (dir . (pattern . EXCLUDE-FILENAMES)))
;;
;; for the 'static type:
;; dir is a path to a dir (string) e.g. "~/musics/"
;; FILENAMES is a list of filenames (string, without path) in the list
;; (file1 file2 file3 ...)
;;
;; for the 'glob type:
;; dir is the same as 'static type.
;; pattern is a glob pattern. for example, "*.mp3".
;; EXCLUDE-FILENAMES is a list of filenames (string, without path) excluded from
;; the list.
;; (excl-file1 excl-file2 excl-file3 ...)
;;
;; the 'regexp type is reserved, but not implemented.

(defvar dog-default-list
  (list dog-default-list-name t nil nil nil)
  "the initial default list")

(defvar dog-list-playing nil
  "whether the list is playing. possible values are `nil' not playing; `t' playing; `pause' paused.")

(defvar dog-list nil
  "the huge list")
(defvar dog-list-name nil
  "list name (string)")
(defvar dog-list-loop-on)
(defvar dog-list-random-on)
(defvar dog-list-last-played-file)
(defvar dog-list-entries)

;;============================
;; dog-list-validate-dog-list
;;============================

(defun dog-list-validate-dog-list ()
  "validate `dog-list' signal error if it's very wrong. used both for debug purpose and real validate. return t if ok; return nil if bad."
  (interactive)
  t)

;;=======================
;; dog-list-insert-texts
;;=======================

(defun dog-list-insert-header-line ()
  "insert header line.

uses `dog-list-name' `dog-list-loop-on' `dog-list-random-on' and global `dog-loop-on' `dog-random-on'."
  (interactive)
  (insert "list: "
	  dog-list-name
	  (if dog-list-playing
	      (if (equal dog-list-playing 'pause)
		  "[paused] "
		"[playing] "))
	  (if (or (equal dog-list-loop-on 'yes)
		  (and (null dog-list-loop-on)
		       dog-loop-on))
	      "[loop] ")
	  (if (or (equal dog-list-random-on 'yes)
		  (and (null dog-list-random-on)
		       dog-random-on))
	      "[random] ")
	  "\n"))

(defsubst dog-list-insert-indent-for-entry ()
  (insert (make-string (+ dog-pre-line-spaces
			  dog-dir-indent)
		       ?\ )))

(defsubst dog-list-insert-indent-for-file ()
  (insert (make-string (+ dog-pre-line-spaces
			      dog-dir-indent
			      dog-file-indent)
			   ?\ )))

;;TODO how to catch errors in elisp? exceptions?
(defun dog-list-insert-static-entry (dir-files)
  "insert given content of a static entry."
  (interactive)
  (let ((dir (car dir-files)))
    (dog-list-insert-indent-for-entry)
    (insert dir "\n")
    (dolist (file (cdr dir-files))
      (dog-list-insert-indent-for-file)
      (insert file "\n"))))

(defun dog-list-glob-to-regexp (glob)
  "glob only support ? and *. which matches . and .* in regexp.

quote . +
not quoted for now: [ ] ^ $ \
replace ? with .
replace * with .*

TODO use a existing function for this purpose.
"
  (interactive)
  ;; replace order matters
  (let* ((str glob)
	 (str (replace-regexp-in-string "\\." "\\." str nil t))
	 (str (replace-regexp-in-string "\\+" "\\+" str nil t))
	 (str (replace-regexp-in-string "\\?" "." str nil t)))
    (replace-regexp-in-string "\\*" ".*" str nil t)))
;; (equal ".*\\.mp3" (dog-list-glob-to-regexp "*.mp3"))
;; (equal ".*" (dog-list-glob-to-regexp "*"))
;; glob has chars that has special meanings in regexp
;; (equal "\\+_\\+" (dog-list-glob-to-regexp "+_+"))

(defun dog-list-expand-glob-entry (start dir pattern)
  "expand glob entry. insert files matching the pattern in dir at point start.  start should be the beginning of the next line below the entry line. if update-entry is none-nil, update the entry text after expanding."
  (interactive)
  (let ((ro buffer-read-only))
    (if ro
	(setq buffer-read-only nil))

    ;; insert files
    (goto-char start)
    (if (file-exists-p dir)
	(let ((files (directory-files dir nil (dog-list-glob-to-regexp pattern))))
	  (if files
	      (dolist (file files)
		(dog-list-insert-indent-for-file)
		(insert file "\n"))
	    (dog-list-insert-indent-for-file)
	    (insert "[!No matching file]\n")))
      (insert "[!Dir dosn't exist]\n"))

    ;; update entry text
    (search-forward-regexp "^ *\\(g\\) ")
    (replace-match "G" t nil nil 1)

    ;; set back ro status
    (setq buffer-read-only ro)))

(defun dog-list-insert-glob-entry (glob)
  "insert given content of a glob entry. insert the pattern only. don't expand the pattern."
  (interactive)
  (let ((dir (car glob))
	(pattern (cadr glob)))
    (dog-list-insert-indent-for-entry)
    (insert dir pattern "\n")
    (dog-list-expand-glob-entry (point) dir pattern)))

(defun dog-list-insert-entry (entry)
  "insert given entry.

remember to mark `dog-list-last-played-file' if it's in this entry."
  (interactive)
  (case (car entry)
    ('static (dog-list-insert-static-entry (cdr entry)))
    ('glob   (dog-list-insert-glob-entry (cdr entry)))
    ('regexp nil)
    (t       nil)))

(defun dog-list-insert-texts ()
  "insert initial texts to buffer."
  (interactive)
  ;; empty lines
  (insert (make-string dog-pre-buffer-newlines ?\n))

  ;; header line
  (insert (make-string dog-pre-line-spaces ?\ ))
  (dog-list-insert-header-line)

  ;; entries
  (dolist (entry dog-list-entries)
    (dog-list-insert-entry entry)))

;;================
;; font lock mode
;;================

(defvar dog-list-lock-keywords
  '(;; list name
    ("^ *list: \\([^ []+\\)" . (1 'dog-list-name-face))
    ;; error msg [!foo]
    ("\\[![^]]*\\]" . 'dog-list-error-face)
    ;; info msg [foo]
    ("\\[[^]]*\\]" . 'dog-list-info-face)
    ;; current playing file
    ("^>.*" . 'dog-list-current-file-face)
    ;; temporary disabled file
    ("^#.*" . 'font-lock-comment-face)))

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
  (set-local dog-list-name (dog-list-name (buffer-name)))

  (make-local-variable 'dog-list)
  (unless (load-file (dog-list-file dog-list-name))
    (kill-buffer)
    (error "Error reading list file"))
  ;; after loading this file, `dog-list' is a valid dog list object

  ;; check whether the `dog-list' is well formed
  (dog-list-validate-dog-list)

  ;; the 1st is dog-list-name, which is already set.
  (set-local dog-list-loop-on (nth 2 dog-list))
  (set-local dog-list-random-on (nth 3 dog-list))
  (set-local dog-list-last-played-file (nth 4 dog-list))
  (set-local dog-list-entries (nth 5 dog-list))

  ;; add text to buffer, with text properties.
  (dog-list-insert-texts)
  
  ;; font lock mode
  (set-local font-lock-defaults
	     (list 'dog-list-lock-keywords t))

  ;; add key bindings
  (define-key dog-list-mode-map (kbd "C-c C-n")
    'll-next-section)
  (define-key dog-list-mode-map [remap indent-new-comment-line]
    'll-indent-new-comment-line)

  ;; post processing
  (set-local buffer-read-only t)
  (add-to-list 'dog-buffer-list (buffer-name))
  )

(provide 'dog-list)

;;; dog-list.el ends here
