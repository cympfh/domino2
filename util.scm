(use srfi-1)

(define (make-domino x y t)
  (let ((theta t) (vx 0) (vy 0) (omega 0)
        (x1 (- x (* *height/2* (cos t))))
        (x2 (+ x (* *height/2* (cos t))))
        (y1 (- y (* *height/2* (sin t))))
        (y2 (+ y (* *height/2* (sin t)))))
  (lambda args
    (if (= (length args) 1)
        (case (car args)
          ((t theta) theta)
          ((vx) vx) ((vy) vy) ((o omega) omega)
          ((x1) x1) ((y1) y1) ((x2) x2) ((y2) y2))
        (let1 val (cadr args)
          (case (car args)
            ((t theta) (set! theta val))
            ((vx) (set! vx val)) ((vy) (set! vy val))
            ((o omega) (set! omega val))
            ((x1) (set! x1 val)) ((y1) (set! y1 val))
            ((x2) (set! x2 val)) ((y2) (set! y2 val)))) ))))

(define (display-dominos dominos)
  (display "[")
  (for-each
    (lambda (d)
      (print (string-append
                  (number->string (d 'x1)) ","
                  (number->string (d 'y1)) ","
                  (number->string (d 'x2)) ","
                  (number->string (d 'y2)) ",")))
    dominos)
  (display "],"))

(define (read-dominos)
  (let loop ((ac '()))
    (let1 a (read)
    (if (eq? a 'EOF) ac
        (loop (cons (make-domino a (read) (read)) ac))))))

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
