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

(defparameter *filepath* (merge-pathnames "day5.txt"))

(defun process-line (line)
  (loop :for s :in (split-string-with-delimiter line)
        :for i = 0 then (1+ i)
        :when (oddp i)
          :collect (read-from-string s)
        ))

(defun get-from-file ()
    (with-open-file (fs *filepath*)
      (loop :for line = (read-line fs nil 'eof)
            :until (eq line 'eof)
            :collect (process-line line)
            )
      )
  )

(defparameter *crates-test*
  '((N Z)
    (D C M)
    (P)
    ))

(defparameter *crates*
  '((N V C S)
    (S N H J M Z)
    (D N J G T C M)
    (M R W J F D T)
    (H F P)
    (J H Z T C)
    (Z L S F Q R P D)
    (W P F D H L S C)
    (Z G N F P M S D)
    ))

(defun move-one-box (from to)
    (push (pop (nth (1- from) *crates*)) (nth (1- to) *crates*))
    )

(defun sol1 ()
  (mapcar #'car
          (loop :for (n from to) :in (get-from-file)
                :do
                   (dotimes (i n)(move-one-box from to))
                :finally
                   (return *crates*)
                )))

(defun move-boxes-together (n from to)
  (let ((temp (loop :for i = 0 :then (1+ i)
                    :while (< i n)
                    :collect (pop (nth (1- from) *crates*))
                    )))
    (dolist (te (nreverse temp)) (push te (nth (1- to) *crates*) ))
    ))

(defun sol2 ()
  (mapcar #'car
          (loop :for (n from to) :in (get-from-file)
                :do
                   (move-boxes-together n from to)
                :finally
                   (return *crates*)
                )))
