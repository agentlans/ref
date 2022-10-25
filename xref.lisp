(in-package :ref)

;; AREF-style accessors
(defmacro def-accessor (name args form)
  `(progn
     ;; Accessor
     (defun ,name ,args
       ,form)
     ,(with-gensyms (new-val)
	;; SETF accessor
	`(defun (setf ,name) ,(cons new-val args)
	   (setf ,form ,new-val)))))

;; For list
(def-accessor lref (obj i)
  (elt obj i))

;; For hash table
(def-accessor href (obj i &key (default nil))
  (gethash i obj default))

;; For object
(def-accessor oref (obj i)
  (slot-value obj i))

;; For association list (alist)
(def-accessor alist-ref (obj key)
  (cdr (assoc key obj)))
;; Note: (setf (alist-ref foo key) val)
;;    doesn't work if key isn't in the alist.

;; For property list (plist)
(def-accessor plist-ref (obj key &key (default nil))
  (getf obj key default))

;; For functions
(defun fref (obj &rest args)
  "Calls function obj on the arguments.
Note: no (SETF (FREF ...) ...) because that wouldn't make sense."
  (apply #'funcall obj args))
;; (fref 'list 1 2 3)

