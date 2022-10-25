(ql:quickload 'alexandria)
(defpackage :ref
  (:use :cl :alexandria))
(in-package :ref)

(load "xref.lisp")

(defun ref-replacer (ref objs)
  "Replaces (obj args...) with (ref obj args...)
if obj matches any of the given objects."
  (lambda (lst)
    (destructuring-bind (obj . indices)
	lst
      (if (member obj objs)
	  `(,ref ,obj ,@indices)
	  lst))))

(defun with-name (name)
  "Returns the symbol WITH-nameS."
  (read-from-string
   (format nil "with-~As" name)))

(defun recursively-list-only (f)
  "Calls f recursively on x but only on lists that make up x.
Note: from my clispr package."
  (lambda (x)
    (cond ((atom x) x)
	  ((listp x) (funcall f (mapcar (recursively-list-only f) x)))
	  (t x))))

(defmacro def-with-macro (obj-type ref)
  "Generates a macro that uses *REF accessors whenever
the specified objects of a particular type appear in the body code."
  `(defmacro ,(with-name obj-type) (objs &body body)
     (funcall (recursively-list-only
	       (ref-replacer ,ref objs))
	      `(progn ,@body))))
#|
;; Example of a generated WITH-* macro
(defmacro with-objects (objs &body body)
  (funcall (recursively-list-only
	    (ref-replacer 'ref objs))
	   `(progn ,@body)))
|#

;; Define the WITH-* macros that use the *REF accessors like
;; WITH-ARRAYS, WITH-LISTS, WITH-HASH-TABLES
(def-with-macro array 'aref)
(def-with-macro list 'lref)
(def-with-macro hash-table 'href)
(def-with-macro object 'oref)
(def-with-macro alist 'alist-ref)
(def-with-macro plist 'plist-ref)
(def-with-macro function 'fref)

#|
;; Example of using the generated WITH-* macros
(let ((al '((a . 1) (b . 2))))
  (with-alists (al)
    (al 'b)))
|#
