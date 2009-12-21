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
;; (dog-version listname loop-p random-p last-played-file ENTRIES)
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
;; count is a number means the nth entry in ENTRIES
;; filename is the last played filename
;;
;; ENTRIES is a list, each element in the list can be one of the following
;;
;; (('static . dir) FILENAMES COMMENTED)
;; (('glob . pattern) EXCLUDE-FILENAMES COMMENTED)
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

(eval-when-compile
  (require 'dog-common)
  (require 'dog-utils))

(defvar dog-list-playing nil
  "whether the list is playing. possible values are `nil' not playing; `t' playing; `pause' paused.")

(defvar dog-list nil
  "the huge list")
(defvar dog-list-name nil)
(defvar dog-list-file nil)
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
  (let ((left (concat "list: "
		      dog-list-name
		      (if dog-list-playing
			  (if (equal dog-list-playing 'pause)
			      "[paused] "
			    "[playing] "))))
	(right (concat (if (or (equal dog-list-loop-on 'yes)
			       (and (null dog-list-loop-on)
				    dog-loop-on))
			   "[loop] ")
		       (if (or (equal dog-list-random-on 'yes)
			       (and (null dog-list-random-on)
				    dog-random-on))
			   "[random] "))))
    (let ((leftw (string-width left))
	  (rightw (string-width right))
	  (totalw (window-width)))
      ;; .................................
      ;; [spc] left [spc?]  right [margin]
      (insert left
	      (make-string (- totalw
			      dog-pre-line-spaces
			      leftw
			      rightw
			      dog-right-margin)
			   ?\ )
	      right
	      "\n\n"))))

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
see `wildcard-to-regexp'
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
  (edit-readonly-maybe
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
    (replace-match "G" t nil nil 1)))

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
  (if dog-list-entries
      (dolist (entry dog-list-entries)
	(dog-list-insert-entry entry))
    (message "list is empty. press a to add file to list.")))

;;================
;; player control
;;================

(defun dog-play-file ()
  "play file at point.

this function may update `dog-last-active-list'."
  (interactive)
  ;; check whether file exists etc.
  ;; if success,
  (message "play file: now playing")
  (setq dog-active-buffer (buffer-name))
  (setq dog-last-active-list dog-list-name))

;; when play pause

;;===============================
;; list control helper functions
;;===============================

(defun dog-list-add-file-plain (file)
  "add given file to `dog-list-entries'"
  (interactive)
  (let ((f (expand-file-name file)))
    (let ((dir (file-name-directory f))
	  (file (file-name-nondirectory f)))
      (if (dog-merge-static-file dir file)
	  
  )

(defun dog-list-add-file-glob-pattern (pattern)
  "add given pattern to `dog-list-entries'"
  (interactive)
  )

;;==============
;; list control
;;==============

(defun dog-list-add-file (file)
  "add file to list. file can be a complete file name (string) or a glob pattern (string)."
  (interactive "GAdd file: ")
  
  (if (file-exists-p file)
      ;; complete file name
      (dog-list-add-file-plain file)
    (dog-list-add-file-glob-pattern file))
  ;; update view
  )

(defun dog-list-refresh ()
  "re-expand pattern entries."
  (interactive)
  )

(defun dog-list-remove-non-existing-files ()
  "first do a `dog-list-refresh' to remove non existing files for glob pattern entry. then remove non-existing-files from static dir entry."
  (interactive)
  )

(defun dog-list-rename-list (newname)
  "rename current list to newname.
rename buffer name to match list name.
if a file on disk exists for current list, rename that file as well."
  (interactive "MNew name: ")
  (let ((oldfile dog-list-file))
    ;; update `dog-list-name' and `dog-list-file'
    (setq dog-list-name newname)
    (setq dog-list-file (dog-list-file newname))

    ;; rename buffer and update `dog-buffer-list'
    (remove-from-list 'dog-buffer-list (buffer-name))
    (rename-buffer (dog-buffer-name newname))
    (add-to-list 'dog-buffer-list (buffer-name))

    ;; update view
    (edit-readonly-maybe
     (save-excursion
       (goto-char (point-min))
       (if (re-search-forward dog-list-pattern-list-name)
	   (replace-match newname nil t nil 1))))

    ;; rename list file on disk if there is one
    (if (file-exists-p oldfile)
	(rename-file oldfile dog-list-file))))

;;=============
;; kill-buffer
;;=============

;; things to do when kill dog list buffer
;; write all info back to `dog-list-file'

(defun dog-list-generate-entries ()
  "generate `dog-list-entries' from the text in the buffer. and from the synced variable `dog-list-glob-remove-alist'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\\([DdGg]\\) \\(.*\\)" nil t)
      (let ((type-char (match-string 1))
	    (dir-or-pattern (match-string 2))
	    (file-pattern "^>? *\\([fc]\\) \\(.*\\)")
	    files
	    commented)
	(case type-c
	  (("D" "d")
	   (forward-line)
	   (while (looking-at file-pattern)
	     (let ((file-mark (match-string 1))
		   (file-name (match-string 2)))
	       (setq files (cons file-name files))
	       (if (equal file-mark "c")
		   (setq commented (cons file-name commented))))
	     (forward-line))
	   ;; now we have dir-or-pattern, files, commented
	   (setq dog-list-entries
		 (cons (list (cons 'static dir-or-pattern) files commented)
		       dog-list-entries)))
	  ("g" ;; now we have 'glob, dir-or-pattern
	   (setq dog-list-entries
		 (cons (list (cons 'glob dir-or-pattern) nil nil)
		       dog-list-entries)))
	  ("G"
	   ;; connect 'c' lines to commented var
	   (forward-line)
	   (while (looking-at file-pattern)
	     (let ((file-mark (match-string 1))
		   (file-name (match-string 2)))
	       (if (equal file-mark "c")
		   (setq commented (cons file-name commented))))
	     (forward-line))
	   ;; make use of `dog-list-glob-remove-alist'
	   (setq dog-list-entries
		 (cons (list (cons 'glob dir-or-pattern)
			     (assoc dir-or-pattern dog-list-glob-remove-alist)
			     commented)
		       dog-list-entries))))))))

(defun dog-list-kill-buffer ()
  "write list to file then `kill-buffer'"
  (interactive)
  (dog-list-generate-entries)
  (let ((listbuf (current-buffer))
	(listfile dog-list-file)
	(bufname (buffer-name))
	(buf (generate-new-buffer dog-list-name))
	(str (format "(setq dog-list\n  (list \"%s\" \"%s\" %s %s '%s '%s))\n\n"
		     dog-version
		     dog-list-name
		     dog-list-loop-on
		     dog-list-random-on
		     dog-list-last-played-file
		     dog-list-entries
		     )))
    (princ str buf)
    ;; write buffer content to file
    (set-buffer buf)
    (write-file listfile)
    ;; kill temp buffer
    (kill-buffer buf)
    (if (equal dog-active-buffer bufname)
	(setq dog-active-buffer nil))
    (remove-from-list 'dog-buffer-list bufname)
    (if (null dog-buffer-list)
	;; this is the last dog list buffer, save global dog config.
	(dog-write-config))
    ;; kill dog list buffer
    (kill-buffer listbuf)))

;;================
;; font lock mode
;;================

(defvar dog-list-pattern-list-name "^ *list: \\([^ []+\\)"
  "used in `dog-list-lock-keywords' `dog-list-rename-list'")

(defvar dog-list-lock-keywords
  '(;; list name
    (dog-list-pattern-list-name . (1 'dog-list-name-face))
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
  "Major mode to play and manage a single dog play list.
\\{dog-list-mode-map}
Turning on dog-list mode runs the normal hook `dog-list-mode-hook'."
  :syntax-table nil
  :abbrev-table nil
  :group 'dog

  ;; TODO what happens if I do M-x dog-list on a dog-list buffer.
  ;; I don't want to reload anything. just do a 'g' command.

  (when (local-variable-p dog-list-name)
    (dog-list-refresh)
    ;; is it ok using error to return
    (error ""))

  ;; now all we have is buffer name. we will build everything from here.
  (set-local dog-list-name (dog-list-name (buffer-name)))
  (set-local dog-list-file (dog-list-file dog-list-name))
  
  (make-local-variable 'dog-list)
  (if (file-exists-p dog-list-file)
      (unless (load-file dog-list-file)
	(kill-buffer)
	(error "Error reading list file %s" dog-list-file))
    ;; create an empty list
    (setq dog-list
	  (list dog-version dog-list-name nil nil nil nil)))
  
  ;; after loading this file, `dog-list' is a valid dog list object
  (add-to-list 'dog-buffer-list (buffer-name))

  ;; check whether the `dog-list' is well formed
  (dog-list-validate-dog-list)

  ;; the 1st is dog-list-name, which is already set.
  (set-local dog-list-loop-on (nth 3 dog-list))
  (set-local dog-list-random-on (nth 4 dog-list))
  (set-local dog-list-last-played-file (nth 5 dog-list))
  (set-local dog-list-entries (nth 6 dog-list))

  ;; add text to buffer, with text properties.
  ;; TODO if last-played-file is found, use > to mark that file.
  (dog-list-insert-texts)

  ;; free up `dog-list-entries'. now all ops will be based on text.
  (setq dog-list-entries nil)
  
  ;; font lock mode
  (set-local font-lock-defaults
	     (list 'dog-list-lock-keywords t))

  ;; add key bindings
  (define-key dog-list-mode-map (kbd "q")
    'quit-window)
  (define-key dog-list-mode-map (kbd "Q")
    'dog-list-kill-buffer)

  (define-key dog-list-mode-map (kbd "a")
    'dog-list-add-file)
  (define-key dog-list-mode-map (kbd "d")
    'dog-list-delete-file)
  (define-key dog-list-mode-map (kbd "D")
    'dog-list-delete-file-at-disk)
  (define-key dog-list-mode-map (kbd "r")
    'dog-list-rename-file)
  (define-key dog-list-mode-map (kbd "R")
    'dog-list-rename-file-at-disk)
  
  (define-key dog-list-mode-map [remap comment-dwim]
    'dog-list-comment-line)
  
  ;; post processing
  (set-local buffer-read-only t)
  (add-to-list 'dog-buffer-list (buffer-name))
  )

(provide 'dog-list)

;;; dog-list.el ends here
