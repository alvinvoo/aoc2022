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
