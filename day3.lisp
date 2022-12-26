
(defparameter *filepath* (merge-pathnames "day3.txt"))

(defun get-from-file ()
    (with-open-file (fs *filepath*)
      (loop :for line = (read-line fs nil 'eof)
            :until (eq line 'eof)
            :for len = (/ (length line) 2)
            :collect (list (subseq line 0 len) (subseq line len))
            )
      )
  )

(defun get-intersect (l1 l2)
  (let ((ls '()))
    (loop :for x :across l1
          :for ret = (loop :for y :across l2
                           :when (char= x y)
                             :return x
                           )
          :when (not (eq nil ret))
            :do
               (setf ls (adjoin ret ls))
          :finally
           (return ls)
          )
    ))

(defun convert-char (c)
  (let ((ccode (char-code c)))
    (if (>= ccode 97) ;; small caps
        (- ccode 96)
        (- ccode 38)
        )
    ))

(defun sol1 ()
  (apply #'+
         (map 'list #'(lambda (l) (convert-char
                                   (car
                                    (get-intersect (car l) (cadr l))))
                        ) (get-from-file))
         )
  )

(defun get-from-file2 ()
  (let ((lss '()) (i 0))
    (with-open-file (fs *filepath*)
      (loop :for line = (read-line fs nil 'eof)
            :until (eq line 'eof)
            :do
               (case i (0 (progn
                            (setf lss (list line))
                            (incf i)
                            ))
                     (1 (progn
                          (setf lss (append lss (list line)))
                          (incf i)
                          ))
                     (otherwise (progn
                                  (setf lss (append lss (list line)))
                                  (setf i 0)
                                  ))
                     )
              :when (= i 0)
              :collect lss :into ret
            :finally
               (return ret)
            )
      ))
  )

(defun pf (l)
  (loop :for (x y z) :in l
        :collect
            (car (get-intersect (concatenate 'string (get-intersect x y)) z))
        ))

(defun sol2 ()
  (apply #'+ (mapcar #'convert-char (pf (get-from-file2)
   ))))
