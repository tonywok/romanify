(defparameter *numerals* (list (cons 1000 "M") (cons 500 "D") (cons 100 "C")
                               (cons 50 "L") (cons 10 "X") (cons 5 "V")))

(defun roman-for (num)
  (cdr (assoc num *numerals*)))

(defun base-numbers ()
  (mapcar #'car *numerals*))

(defun romanify (arabic-num)
  (labels ((add-Is (num str)
                   (if (eq 0 num) str
                     (add-Is (- num 1) (concatenate 'string str "I"))))

           (I-before-base (str base)
                          (concatenate 'string str "I" (roman-for base)))

           (build (bases num str)
                  (let* ((base (car bases))
                         (left-over (mod num base)))
                    (cond ((eq num 0) str) ; return str when num is 0 - done
                      ((eq num base) ; num equals base - add base
                        (build (cdr bases) left-over (concatenate 'string str (roman-for base))))
                      ((eq num (- base 1)) ; num is one less than base - add I before base
                        (build (cdr bases) left-over (I-before-base str base)))
                      ((> num (- base 1)) ; num is more than two less than base - go a base lower
                        (build (cdr bases) num str))
                      (T (build (cdr bases) left-over (add-Is left-over str)))))))
    (build (base-numbers) arabic-num "")))
