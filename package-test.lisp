(in-package :cl)

(defpackage :package-a
  (:use :cl)
  (:export *objects* put get)
  (:shadow get))

(in-package :package-a)

(defvar *objects* (make-hash-table)
  "Container for dynamically generated objects we want to expose to the
  package's user.")

(defun put (name obj)
  (setf (gethash name *objects*) obj))

(defun get (name &optional default)
  (gethash name *objects* default))

;; Your code can put arbitrary objects into the hash table
(put :foo (lambda () :a-thunk))
(put :bar (lambda () :another))

;; And Your users can retrieve them
(in-package :cl-user)
(use-package :package-a)
(funcall (get :foo "abc")) ;; => :a-thunk
