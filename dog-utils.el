;;; dog-utils is completely independent from dog. it's utils only.

;;==============
;; common utils
;;==============

(defun my-filter (f l)
  "filter"
  (interactive)
  (if (consp l)
    (if (funcall f (car l))
	(cons (car l)
	      (my-filter f (cdr l)))
      (my-filter f (cdr l)))))

(defun remove-from-list (list element)
  "if element is in list remove it. and modify list in place. list is a symbol whose value is a list. this function is ... to `add-to-list'."
  (interactive)
  (set list
       (my-filter (lambda (x) (not (equal element x)))
		  (symbol-value list))))

;; (let ((tmplist '(1 2 3 4)))
;;   (remove-from-list 'tmplist 3)
;;   (equal tmplist '(1 2 4)))

(defmacro edit-readonly-maybe (&rest body)
  "checks `buffer-read-only', set to writable, do body, then set back the old value."
  `(let ((ro buffer-read-only))
     (if ro
	 (setq buffer-read-only nil))
     ,(cons 'progn body)
     ;; set back ro status
     (setq buffer-read-only ro)))

(defmacro set-local (var &optional default)
  "make var a buffer local variable and set it's value to default. var should be a symbol no quote.

this macro is shorthand for (set (make-local-variable 'var) default)."
  `(set (make-local-variable (quote ,var)) ,default))

(provide 'dog-utils)
