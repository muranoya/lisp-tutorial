(defun sqsum (a b)
  (sqrt (+ (* a a) (* b b))))

(defun get-greater (a b)
  (if (> a b) a b))

; print sets of (1<=i<=10, 1<=j<=10, 1<=k<=10)
(defun show-ijk ()
  (dotimes (i 10)
    (dotimes (j 10)
      (dotimes (k 10)
        (let ((ii (+ i 1))
              (jj (+ j 1))
              (kk (+ k 1)))
          (print (list ii jj kk)))))))

; solve ax^2+b+c=0
(defun solve-abc (a b c)
  (let ((t1 (- b))
        (t2 (sqrt (- (* b b) (* 4 a c))))
        (t3 (* 2 a)))
    (values (/ (+ t1 t2) t3) (/ (- t1 t2) t3))))

; a result equals with gcd(built-in function)
(defun my-gcd2 (i j))

; you can use dolist macro
; if you will write a code of tail-recursion version, how to write?
(defun sum-list (xl)
  (if (null xl) 0
    (+ (car xl) (sum-list (cdr xl)))))

; tail-recursion version of sum-list
(defun sum-list-r (xl)
  (sum-list0 xl 0))
(defun sum-list0 (xl a)
  (if (null xl) a
    (sum-list0 (cdr xl) (+ a (car xl)))))

; a code using dolist
(defun filt-nonneg (l)
  (let ((a '()))
    (dolist (i l)
      (if (>= i 0) (push i a)))
    (reverse a)))

; a code using recursion
(defun filt-nonneg-r (l)
  (if (null l) '()
    (let ((head (car l)))
      (if (>= head 0)
        (cons (car l) (filt-nonneg (cdr l)))
        (filt-nonneg-r (cdr l))))))

; a code using tail-recursion
(defun filt-nonneg-tr (l)
  (reverse (filt-nonneg-tr0 l '())))
(defun filt-nonneg-tr0 (l m)
  (if (null l) m
    (let ((head (car l)))
      (if (>= head 0)
        (filt-nonneg-tr0 (cdr l) (cons head m))
        (filt-nonneg-tr0 (cdr l) m)))))

(defun ijk10-list ()
  (let ((l '()))
    (dotimes (i 10)
      (dotimes (j 10)
        (dotimes (k 10)
          (let ((ii (+ i 1))
                (jj (+ j 1))
                (kk (+ k 1)))
            (push (list ii jj kk) l)))))
    l))

(defun sep-symbols (l)
  (if (null l) '()
    (let ((head (car l))
          (tail (cdr l)))
      (cond ((listp head) (multiple-value-bind (a b) (sep-symbols head)
                            (multiple-value-bind (aa bb) (sep-symbols tail)
                              (values (append a aa) (append b bb)))))
            ((symbolp head) (multiple-value-bind (a b) (sep-symbols tail)
                              (values (cons head a) b)))
            (t (multiple-value-bind (a b) (sep-symbols tail)
                 (values a (cons head b))))))))
