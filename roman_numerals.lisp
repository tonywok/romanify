;;; in general, if you want need this in production code, there is already
;;; a FORMAT code for printing roman numerals:
;;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm

(defparameter *numerals* '((1000 . "M") (500 . "D") (100 . "C") (50 . "L") (10 . "X") (5 . "V") (1 . "I")))

(defun roman-for (num)
  (cdr (assoc num *numerals*)))

(defun base-numbers ()
  (mapcar #'car *numerals*))

(defun conc (&rest strings)
  "Concatenates a number of STRINGS."
  (apply #'concatenate 'string strings))

(defun romanify (arabic-num)
  (labels ((add-roman (num str base)
       (if (= 0 num)
     str
     (add-roman (1- num) (conc str (roman-for base)) base)))
     (build (bases num str)
       (let ((base (car bases)))
         (if (= 0 num)
       str                                                            ; base case
       (multiple-value-bind (quotient remainder) (floor num base)
         (cond ((and (> quotient 0) (= remainder 0))                  ; base goes in evenly, append base(s)
                 (build (cdr bases) remainder (add-roman quotient str base)))
               ((and (> quotient 0) (= remainder (1- base)))          ; base goes in evenly, append bases(s) & wierd leftover
                 (conc (add-roman quotient str base) str "I" (roman-for base)))
               ((and (> quotient 0) (< remainder (1- base)))          ; base goes in evenly, handle leftover
                 (build (cdr bases) remainder (add-roman quotient str base)))
               ((and (= quotient 0) (= remainder (1- base)))          ; wierd case (ie IV)
                 (conc str "I" (roman-for base)))
               (T                                                     ; bottom out
                 (build (cdr bases) remainder str))))))))
    (build (base-numbers) arabic-num "")))
