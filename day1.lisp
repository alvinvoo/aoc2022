(defparameter *filepath* (merge-pathnames "day1.txt"))

(defun get-from-file (lss ls)
  (with-open-file (fs *filepath*)
    (loop :for line = (read-line fs nil 'eof)
          :if (or (string= line "") (eq line 'eof))
            :do (progn
                  (setf ls (append ls (list lss)))
                  (setf lss '())
                  )
          :else
            :do (setf lss (append lss (list (read-from-string line))))
          :finally (return ls)
          :until (eq line 'eof)
          )
    ))

(defparameter *file-list* (get-from-file '() '()))
(defun aggre-map-list (file-list) (map 'list (lambda (x) (apply #'+ x)) file-list))
(defun sol1 (file-list)
  (apply #'max (aggre-map-list file-list))
  )
;(sol1 file-list)

(defun update-top3-list (top3-list target ret)
  "will both return and update top3-list in place, ret = (val . pos)"
  (loop :for il :in top3-list
        :for ind :from 0
        :for diff = (- target il)
        :for rr = (car ret)
        :when (> diff rr)
          :do (setf ret (cons target ind))
        :finally
           (return
             (if (> (car ret) 0)
                 (progn
                 (setf (nth (cdr ret) top3-list) (car ret))
                 top3-list
                 )))
        )
  )

(defun sol2 (file-list top3-list)
  (loop :for x :in (aggre-map-list file-list)
        :do (update-top3-list top3-list x '(0 . 0))
        :finally (return top3-list)
        )
  )
;; still wrong, suffer from dependency problem
;; (72718 70450 69219) - 212387
(apply #'+ (sol2 *file-list* (make-list 3 :initial-element 0)))


;; 72718 70450 69921 - 213089
(defun sol2-correct (file-list)
  (apply #'+ (subseq (stable-sort (aggre-map-list file-list) #'>) 0 3))
  )
