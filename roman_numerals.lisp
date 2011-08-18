(defparameter *numerals* (list (cons 1000 "M") (cons 500 "D") (cons 100 "C")
                               (cons 50 "L") (cons 10 "X") (cons 5 "V") (cons 1 "I")))

(defun roman-for (num)
  (cdr (assoc num *numerals*)))

(defun base-numbers ()
  (mapcar #'car *numerals*))

(defun romanify (arabic-num)
  (labels ((add-roman (num str base)
                   (if (eq 0 num)
                     str
                     (add-roman (- num 1) (concatenate 'string str (roman-for base)) base)))

           (build (bases num str)
                  (let ((base (car bases)))
                    ; find the biggest numeral within 1 of num
                    (if (eq 0 num)                                             ; base case
                      str
                      (multiple-value-bind (quotient remainder) (floor num base)
                        (cond ((and (> quotient 1) (eq remainder 0))           ; base goes in evenly
                                (build (cdr bases) remainder (add-roman quotient str base)))
                              ((and (> quotient 1) (not (eq remainder 0)))     ; base goes in evenly, have some left
                                (build (cdr bases) remainder (add-roman quotient str base)))
                              ((and (eq quotient 0) (eq remainder (- base 1))) ; wierd case (ie IV)
                                (concatenate 'string str "I" (roman-for base)))
                              (T (build (cdr bases) remainder str))))))))
    (build (base-numbers) arabic-num "")))
