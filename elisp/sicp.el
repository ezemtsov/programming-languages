(defun square (x) (* x x))

(defun average (x y)
  (/ (+ x y) 2))

(defun sqrt (x)
  (defun good-enough? (guess x)
    (< (abs (- (square guess) x)) 0.001))
  (defun improve (guess x)
    (average guess (/ x guess)))
  (defun sqrt-iter (guess x)
    (if (good-enough? guess x)
        guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt 9)

(sqrt (+ 100 37))

(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        ((not predicate) else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(defun sqrt-iter (guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(defun f-recursive (n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f-recursive (- n 1))
                     (f-recursive (- n 2))
                     (f-recursive (- n 3))))))

(defun f-iterative (n)
  (defun f-iter (a b c count)
    (if (= count 0)
        c
      (f-iter (+ a b c) a b (- count 1))))
  (f-iter 2 1 0 n))

;a <- a + b + c
;b <- a
;c <- b

(f-recursive 7)
(f-iterative 80)
