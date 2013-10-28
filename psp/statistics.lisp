(in-package "ACL2")

; This was recommended by ACL2 to fix an error message
;(SET-DEFAULT-HINTS '((NONLINEARP-DEFAULT-HINT STABLE-UNDER-SIMPLIFICATIONP
 ;                                             HIST PSPV)))

; (sum xs) -- Calculates the sum of the numbers in true-list xs
(defun sum (xs)
  (if (and (cdr xs) (true-listp xs))
      (+ (car xs) (sum (cdr xs)))
      (car xs)))

; (sqr x) -- Calculates the square of a single numeric value x.
(defun sqr (x)
  (* x x))

; (listMinusConst xs c) -- Subtracts a constant value c from each 
;    element in a true-list of numbers.
(defun listMinusConst (xs c)
  (if (and (cdr xs) (true-listp xs))
      (cons (- (car xs) c) (listMinusConst (cdr xs) c))
      (cons (- (car xs) c) nil)))

; (listSqr xs) -- Creates a true-list containing the square of each 
;    element in a true-list of numbers.
(defun listSqr (xs)
  (if (and (cdr xs) (true-listp xs))
      (cons (sqr (car xs)) (listSqr (cdr xs)))
      (cons (sqr (car xs)) nil)))

; (listXlist xs ys) -- Creates a true-list containing the products of
;    corresponding numbers in a pair of true-lists.
(defun listXlist (xs ys)
  (if (and (cdr xs) (true-listp xs) (true-listp ys))
      (cons (* (car xs) (car ys)) (listXlist (cdr xs) (cdr ys)))
      (cons (* (car xs) (car ys)) nil)))

; (mean xs) -- Calculates the mean value of a list of numbers.
(defun mean (xs)
  (/ (sum xs) (length xs)))

; (variance xs) -- Calculates the average variance from the mean of a 
;    list of numbers.
(defun variance (xs)
  (mean (listSqr (listMinusConst xs (mean xs)))))

; (scatter xs ys) -- Calculates the average of the variance between 
;    pairs of x and y coordinates and the respective means of those 
;    coordinate sets.
(defun scatter (xs ys)
  (mean (listXlist (listMinusConst xs (mean xs))
                   (listMinusConst ys (mean ys)))))

; (correlationSquared xs ys) -- Calculates the ratio between the square 
;    of the scatter value of a set of x and y coordinates and the 
;    product of those coordinates' variances.
(defun correlationSquared (xs ys)
  (/ (sqr (scatter xs ys)) (* (variance xs) (variance ys))))

; (beta1 xs ys) -- Calculates the slope of a least-squares line from a
;    collection of points stored as two true-lists of x and y
;    coordinates.
(defun beta1 (xs ys)
  (/ (scatter xs ys) (variance xs)))

; (beta0 xs ys) -- Calculates the y-intercept of the above 
;    least-squares line.
(defun beta0 (xs ys)
  (- (mean ys) (* (beta1 xs ys) (mean xs))))

; Takes the ceiling(log2(num)).
(defun log2-ceiling (num)
  (if (not (posp (- num 1)))
      0
      (+ (log2-ceiling (ceiling num 2)) 1)))

; Find an m such that y = x/4^m where 1/4 < y <= 1.
(defun range-reduce (x)
  (ceiling (log2-ceiling (ceiling x 1)) 2))

; Find the square root of x to one significant digit.
(defun sqrt-to-one-digit (x)
  (let* ((m (range-reduce x))
         (y (/ x (expt 4 m))))
    (* (/ (+ (* 2 y) 1) 3) (expt 2 m))))

; algorithm taken from http://mitpress.mit.edu/sicp/chapter1/node9.html
; Takes the square root of something using newton's method.
(defun newton-sqrt (n x y)
  (if (zp n)
      y
      (newton-sqrt (- n 1) x (/ (+ (/ x y) y) 2))))

; Guarantees n digits of presicion for the square root function.
(defun sqrt~ (n x)
  (newton-sqrt (log2-ceiling n) x (sqrt-to-one-digit x)))


(defun variance-sum (xs ys b0 b1)
  (if (consp xs)
      (+ (sqr (+ (first ys)
                 (- b0)
                 (- (* b1 (first xs)))))
         (variance-sum (rest xs) (rest ys) b0 b1))
      0))

(defconst *p-value-table* 
  '((1 (0 1584/10000 3249/10000 5095/10000 7265/10000 1 1376/1000 1963/1000 3078/1000 6314/1000))
    (2 (0 1421/10000 2887/10000 4447/10000 6172/10000 8165/10000 1061/1000 1386/1000 1886/1000 292/100))
    (3 (0 1366/10000 2767/10000 4242/10000 5844/10000 7649/10000 9785/10000 125/100 1638/1000 2353/1000))
    (4 (0 1338/10000 2707/10000 4142/10000 5686/10000 7407/10000 941/1000 119/100 1533/1000 2132/1000))
    (5 (0 1322/10000 2672/10000 4082/10000 5594/10000 7267/10000 9195/10000 1156/1000 1476/1000 2015/1000))
    (6 (0 1311/10000 2648/10000 4043/10000 5534/10000 7176/10000 9057/10000 1134/1000 144/100 1943/1000))
    (7 (0 1303/10000 2632/10000 4015/10000 5491/10000 7111/10000 896/1000 1119/1000 1415/1000 1895/1000))
    (8 (0 1297/10000 2619/10000 3995/10000 5459/10000 7064/10000 8889/10000 1108/1000 1397/1000 186/100))
    (9 (0 1293/10000 261/1000 3979/10000 5435/10000 7027/10000 8834/10000 11/10 1383/1000 1833/1000))
    (10 (0 1289/10000 2602/10000 3966/10000 5415/10000 6998/10000 8791/10000 1093/1000 1372/1000 1812/1000))
    (15 (0 1278/10000 2579/10000 3928/10000 5357/10000 6912/10000 8662/10000 1074/1000 1341/1000 1753/1000))
    (20 (0 1273/10000 2567/10000 3909/10000 5329/10000 687/1000 86/100 1064/1000 1325/1000 1725/1000))
    (30 (0 1267/10000 2556/10000 389/1000 53/100 6828/10000 8538/10000 1055/1000 131/100 1697/1000))
    (100000 (0 1257/10000 2533/10000 3853/10000 5244/10000 6745/10000 8416/10000 1036/1000 1282/1000 1645/1000))))


; Certainty index i cooresponds with (i*10) percent certainty
(defun t-value-row (degrees-of-freedom p-value-table)
  (let* ((p-value-row (first p-value-table))
         (row (first p-value-row))
         (values (second p-value-row)))
    (if (or (<= degrees-of-freedom row) (endp (rest p-value-table)))
        values
        (t-value-row degrees-of-freedom (rest p-value-table)))))

(defun calc-delta-without-t-value (estimates actuals prediction)
  (let* ((b0 (beta0 estimates actuals))
         (b1 (beta1 estimates actuals))
         (n (length estimates))
         (var (/ (variance-sum estimates actuals b0 b1)
                 (- n 2)))
         (standard-dev (sqrt~ 3 var))
         (xavg (mean estimates))
         (prediction-xavg2 (sqr (- xavg prediction)))
         (xi-xavg (listMinusConst estimates xavg))
         (sum-xi-xavg2 (sum (listSqr xi-xavg)))
         (range (* standard-dev
                   (sqrt~ 3 (+ 1
                               (/ n)
                               (/ prediction-xavg2
                                  sum-xi-xavg2))))))
    range))

(defun prediction-ranges-helper (projection delta t-values)
  (if (consp t-values)
      (cons (cons (- projection (* delta (first t-values)))
                  (+ projection (* delta (first t-values))))
            (prediction-ranges-helper projection delta (rest t-values)))
      nil))

;-----------------Predict Size-----------------------------------
;e - Historical size estimates: e1,e2,e3,....en
;s - Historical actual size: s1, s2, s3, .... sn
;x = size estimated for the current project from the file i/o
;returns the size of the new project.
(defun predict-size(e s x)
  (let ((b0 (beta0 e s))
        (b1 (beta1 e s)))
    (+ b0 (* b1 x))))

(defun prediction-ranges (estimates actuals prediction)
  (let ((delta (calc-delta-without-t-value estimates actuals prediction))
        (t-values (t-value-row (- (length estimates) 2)
                               *p-value-table*))
        (projection (predict-size estimates actuals prediction)))
    (prediction-ranges-helper projection delta t-values)))