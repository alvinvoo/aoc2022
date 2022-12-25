;; first column
;; A - rock B - paper C - scissor
;; second column
;; X - rock Y - paper Z - scissor
;; scores
;; 1 2 3
;; 0 3 6

(defparameter *filepath* (merge-pathnames "day2.txt"))

(defun get-from-file (ls)
  (with-open-file (fs *filepath*)
    (loop :for line = (read-line fs nil 'eof)
          :until (eq line 'eof)
          :do
             (setf ls (append ls (list
                                  (read-from-string
                                   (concatenate 'string "(" line ")"))
                                  )))
          :finally
             (return ls)
          )
    ))

(defparameter *data-list* (get-from-file '() ))

(defun calc-my-hand (me)
  (case me ('X 1)
           ('Y 2)
           ('Z 3)
   ))
(defun calc-win-lose (opp me)
  (cond ((and (eq opp 'A)(eq me 'X)) 3)
        ((and (eq opp 'B)(eq me 'Y)) 3)
        ((and (eq opp 'C)(eq me 'Z)) 3)
        ((and (eq opp 'A)(eq me 'Y)) 6)
        ((and (eq opp 'A)(eq me 'Z)) 0)
        ((and (eq opp 'B)(eq me 'X)) 0)
        ((and (eq opp 'B)(eq me 'Z)) 6)
        ((and (eq opp 'C)(eq me 'X)) 6)
        ((and (eq opp 'C)(eq me 'Y)) 0)
        )
  )
(defun sol1 (data-list)
  (loop :for (opp me) :in data-list
        :sum
           (+ (calc-my-hand me) (calc-win-lose opp me))
   ))

;;(sol1 *data-list*)
;;
;; A - rock B - paper C - scissor
;; X - lose Y - draw Z - win
(defun calc-result (res)
  (case res ('X 0)
            ('Y 3)
            ('Z 6)
        )
  )
(defun calc-my-hand2 (opp res)
  (cond ((and (eq opp 'A)(eq res 'X)) 3)
        ((and (eq opp 'A)(eq res 'Y)) 1)
        ((and (eq opp 'A)(eq res 'Z)) 2)
        ((and (eq opp 'B)(eq res 'X)) 1)
        ((and (eq opp 'B)(eq res 'Y)) 2)
        ((and (eq opp 'B)(eq res 'Z)) 3)
        ((and (eq opp 'C)(eq res 'X)) 2)
        ((and (eq opp 'C)(eq res 'Y)) 3)
        ((and (eq opp 'C)(eq res 'Z)) 1)
   ))

(defun sol2 (data-list)
  (loop :for (opp res) :in data-list
        :sum
           (+ (calc-result res) (calc-my-hand2 opp res))
   ))

(sol2 *data-list*)
