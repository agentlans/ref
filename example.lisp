(ql:quickload 'ref)

(defpackage :foo
  (:use :cl :ref))
(in-package :foo)

(let ((ht (make-hash-table)))
  ;; Reference a hash table key by using the hash table's name.
  ;; That is, don't have to GETHASH every time.
  (with-hash-tables (ht)
     (setf (ht 'spam) 1.0)
     (setf (ht 'eggs) 2.0)
     (ht 'spam)))

;; This package even works with functions. No need for FUNCALL.
(defun hello (name) 
  (format t "Hello ~A" name))
;; WITH-FUNCTIONS
(with-functions ('hello) ; note the quote
  (hello "world"))

