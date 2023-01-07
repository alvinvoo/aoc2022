(defvar *filepath* "day6.txt")

(defun get-from-file ()
  (with-open-file (fs *filepath*)
    (loop :for line = (read-line fs nil 'eof)
          :until (eq line 'eof)
          :return line
          )
    )
  )

(defun convert-to-hash-table (str)
  (let ((hT (make-hash-table)))
    (loop :for s :across str
          :when (not (null s))
          :if (gethash s hT)
            :do (setf (gethash s hT) (1+ (gethash s hT)))
          :else
            :do (setf (gethash s hT) 1)
          :finally (return hT)
          )))

(defun more-than-one (hT)
  (loop :for v :being each hash-values :of hT
        :when (> v 1)
          :return T
        :finally
          (return NIL)))

(defun sol (msg-len)
  (let ((str (get-from-file)))
    (loop :for i = 0 :then (1+ i)
          :for j = msg-len :then (+ i msg-len)
          :while (and (<= j (length str))
                      (more-than-one (convert-to-hash-table (subseq str i j))))
          :finally (return j)
          )
    ))

(sol 4)
(sol 14)
