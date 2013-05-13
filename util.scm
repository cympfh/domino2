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
    (if (eof-object? a) ac
        (loop (cons (make-domino a (read) (read)) ac))))))

(define (center-of-mass d)
  (values
    (/ (+ (d 'x1) (d 'x2)) 2)
    (/ (+ (d 'y1) (d 'y2)) 2)))

(define (foot d)
  (if (< (d 'y1) (d 'y2))
      (values (d 'x1) (d 'y1))
      (values (d 'x2) (d 'y2))))

(define (distance x1 y1 x2 y2)
  (sqrt
    (apply +
      (map (lambda (x) (expt x 2))
        (map - (list x1 y1) (list x2 y2))))))

(define (accel! d fx fy x y)
  (inc! d 'vx (* fx */M* *dt*))
  (inc! d 'vy (* fy */M* *dt*))
  (receive (gx gy) (center-of-mass d)
  (inc! d 'omega (* */I* *dt* (cross (- x gx) (- y gy) fx fy)))))

(define (away->< d e)
  (receive (fx/d _) (foot d)
  (receive (fx/e _) (foot e)
  (if (= fx/d fx/e) (set! fx/d (- fx/d 0.001)))
  (let ((dx/d (if (< fx/d fx/e) -0.1 0.1))
        (dx/e (if (< fx/d fx/e) 0.1 -0.1)))
  (let while ()
    (when (intersect d e)
      (inc! d 'x1 dx/d)
      (inc! d 'x2 dx/d)
      (inc! e 'x1 dx/e)
      (inc! e 'x2 dx/e)
      (while)))))))
