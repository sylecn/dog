;;; common parts for dog, dog-list and dog-lists

(defgroup dog nil
  "dog is a simple music player based on mplayer."
  :tag "dog"
  :version 23.1
  :group 'multimedia)

(defconst dog-version "1.0dev"
  "version number of dog.")

(defconst dog-config-file-name "config.el")

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

;;=================
;; global variables
;;=================

(defvar dog-buffer-list nil
  "contains all opened list buffers")
;;TODO when dog list buffer is killed, update this variable.

(defvar dog-active-buffer nil
  "the active list (string).\nthis is the target buffer for global add file or play control.")

(defvar dog-pre-buffer-newlines 4)
(defvar dog-pre-line-spaces 4)
(defvar dog-dir-indent 4)
(defvar dog-file-indent 4)
(defvar dog-right-margin 4)

;; patterns
(defvar dog-pattern-dir-or-pattern "\\([DdGg]\\) \\(.*\\)")
(defvar dog-pattern-file "^>? *\\([fc]\\) \\(.*\\)"
  "used in `looking-at'. don't remove ^..")
(defvar dog-pattern-dir "\\([DdGg]\\) \\(.*/\\)")

;;======================================
;; configs that preserve cross sessions
;;======================================

;; what's under ~/.emacs.d/dog/
;; config.el example:
;; (setq dog-last-active-list "default")
;; (setq dog-file-display-name-alist nil)
;;

(defvar dog-last-active-list nil
  "the name (string) of last active list.\nlist in this variable will be opened when M-x dog. if it's nil, the default list will be opened.")

(defvar dog-file-display-name-alist nil
  "pairs of (dir . ((filename . display-name) ...))")

;;================
;; base functions
;;================

(defun dog-buffer-name (l)
  "return the buffer name for given list name (string)"
  (concat " dog-" l))

(defun dog-list-name (buf)
  "return the list name of given buffer name (string)"
  (interactive)
  (substring buf 5))

(defun dog-list-file (l)
  "return the config file name for given list name (string)"
  (interactive)
  (concat dog-config-dir l ".el"))

;;======================
;; read write config.el
;;======================

(defsubst dog-make-config-dir ()
  (if (not (file-directory-p dog-config-dir))
      (make-directory dog-config-dir t)))

(defun dog-read-config ()
  "read config from `dog-config-dir' if it exists.\n
currently in config file:
`dog-last-active-list'"
  (interactive)
  (dog-make-config-dir)
  (let ((config (concat dog-config-dir dog-config-file-name)))
    (if (file-exists-p config)
	(load-file config))))

(defun dog-write-config ()
  "write config to `dog-config-dir'. see `dog-read-config' for what to write."
  (interactive)
  (dog-make-config-dir)
  (let ((config (concat dog-config-dir dog-config-file-name)))
    (let ((buf (generate-new-buffer dog-config-file-name)))
      ;; `dog-last-active-list'
      (if dog-last-active-list
	  (princ (format "(setq dog-last-active-list \"%s\")\n\n"
			 dog-last-active-list)
		 buf))
      (if dog-file-display-name-alist
	  (princ (format "(setq dog-file-display-name-alist '%s)\n\n"
			 dog-file-display-name-alist)
		 buf))
      ;; write buf content to file
      (set-buffer buf)
      (write-file config)
      (kill-buffer buf))))


(provide 'dog-common)
