(+ 1 1)

(defun foo (n) (if (plusp n) (* n (bar (1- n))) 1))
(defun bar (n) (if (plusp n) (* n (foo (1- n))) 1))

(defun z () (pprint "abcde"))
(z)

(loop :repeat 10 :do (print "hello world"))
(defvar af (loop :repeat 10 :collect (print "hello world")))

(loop :for i :from 0 :to 10
      :collect (print i))

(loop :for i :from 0 :to 10
      :collect i)

(loop :for i :from 0 :to 10
      :collect (+ i 1))

(loop :for i :from 0 :to 10
      :do (print i)
      :collect (+ i 1))

(loop :for i :from 0 :to 10
      :for j :from 11 :to 1
      :do (format t "~a+~a " i j)
      :collect (+ i j))

(loop :for i :from 0 :to 10
      :for j :from 11 :downto 1
      :do (format t "~%~a+~a " i j)
      :collect (+ i j))

(print (loop :for i :from 0 :to 10 :collect i))


(loop :for i :across #(0 1 2 3 4 5 6 7 8 9 10) ; # is an array, use across
      :for j :in     '(6 9 5 1 3 5 4 8 9 7 10)
      :when (= i j)
        :collect (cons i j)
      :when (> i j)
        :collect (+ i j))

; destructuring in lisp
(loop :for (a . b) :in '((6 . 9) (5 . 1) (3 . 5) (4 . 0) (7 . 4))
      :collect (+ a b))

(loop :for (a . b) :in '((6 . 9) (5 . 1) (3 . 5) (4 . 0) (7 . 4))
      :sum (+ a b))

(loop :for (a nil . b) :in '((6 4 . 9) (5 4 . 1) (3 4 . 5) (4 4 . 0) (7 4 . 4))
      :print (cons a b))

; take note of the difference between keyword and function
(defun m2 (xs)
             (loop :for i :from 0 :to 10 :collect i :into xs :finally (return xs))
           )
