(defun split-string-with-delimiter (string
                                    &key (delimiter #\ )
                                         (keep-delimiters nil)
                                    &aux (l (length string)))
  (loop for start = 0 then (1+ pos)
        for pos   = (position delimiter string :start start)

        ; no more delimiter found
        when (and (null pos) (not (= start l)))
        Collect (subseq string start)

        ; while delimiter found
        while pos

        ;  some content found
        when (> pos start) collect (subseq string start pos)
        ;  optionally keep delimiter
        when keep-delimiters collect (string delimiter)))

(defparameter *filepath* (merge-pathnames "day4.txt"))

(defun process-line (line)
  (mapcar #'(lambda (l)
              (mapcar #'read-from-string
                      (split-string-with-delimiter l :delimiter #\- :keep-delimiters nil)))
          (split-string-with-delimiter line :delimiter #\, :keep-delimiters nil)))

(defun get-from-file ()
    (with-open-file (fs *filepath*)
      (loop :for line = (read-line fs nil 'eof)
            :until (eq line 'eof)
            :collect (process-line line)
            )
      )
  )

(defun contain-within (l1 l2)
  (and (>= (car l1) (car l2))
      (<= (cadr l1) (cadr l2)))
  )

(defun contain-without (l1 l2)
  (and (<= (car l1) (car l2))
      (>= (cadr l1) (cadr l2)))
  )

(defun sol1 ()
  (loop :for (l1 l2) :in (get-from-file)
        :count (or (contain-within l1 l2) (contain-without l1 l2))
   ))

(defun overlap-at-all (l1 l2)
  (and (>= (cadr l2) (car l1))
       (>= (cadr l1) (car l2))
       ))

(defun sol2 ()
  (loop :for (l1 l2) :in (get-from-file)
        :count (or (contain-within l1 l2) (contain-without l1 l2) (overlap-at-all l1 l2))
   ))
