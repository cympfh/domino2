(define (intersect d e)
  (let ((x1 (d 'x1)) (y1 (d 'y1)) (x2 (d 'x2)) (y2 (d 'y2))
        (x3 (e 'x1)) (y3 (e 'y1)) (x4 (e 'x2)) (y4 (e 'y2)))

  ; rought check
	(if (or
					(if (<= x1 x2)
							(or (and (<= x2 x3) (<= x2 x4))
									(and (>= x1 x3) (>= x1 x4)))
							(or (and (<= x1 x3) (<= x1 x4))
									(and (>= x2 x3) (>= x2 x4))))
					(if (<= y1 y2)
							(or (and (<= y2 y3) (<= y2 y4))
									(and (>= y1 y3) (>= y1 y4)))
							(or (and (<= y1 y3) (<= y1 y4))
									(and (>= y2 y3) (>= y2 y4))))
					(= y1 y2 y3 y4))
	    #f
		  
  (and
    (> 0 (* (ccw x1 y1 x2 y2 x3 y3)
            (ccw x1 y1 x2 y2 x4 y4)))
    (> 0 (* (ccw x3 y3 x4 y4 x1 y1)
            (ccw x3 y3 x4 y4 x2 y2)))))))

(define (find-intersect d e)
  (let ((x1 (d 'x1)) (y1 (d 'y1)) (x2 (d 'x2)) (y2 (d 'y2))
        (x3 (e 'x1)) (y3 (e 'y1)) (x4 (e 'x2)) (y4 (e 'y2)))
  (let1 a
        (/ (- (* (- x2 x4) (- y3 y4)) (* (- y2 y4) (- x3 x4)))
           (- (* (- y1 y2) (- x3 x4)) (* (- y3 y4) (- x1 x2))))
  (values
    (+ (* a (- x1 x2)) x2)
    (+ (* a (- y1 y2)) y2)))))
