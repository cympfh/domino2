(use srfi-1)

(define-syntax inc!
  (syntax-rules ()
    ((_ d sym diff) (d sym (+ (d sym) diff)))))

(define-syntax let1
  (syntax-rules ()
    ((_ a x body ...) (let ((a x)) body ...))))

(define (make-domino x y t)
  (let ((x x) (y y) (theta t)
        (vx 0) (vy 0) (omega 0)
        (x1 0) (x2 0) (y1 0) (y2 0))
  (lambda args
    (if (= (length args) 1)
        (case (car args)
          ((x) x) ((y) y) ((t theta) theta)
          ((vx) vx) ((vy) vy) ((o omega) omega)
          ((x1) x1) ((y1) y1) ((x2) x2) ((y2) y2))
        (let1 val (cadr args)
          (case (car args)
            ((x) (set! x val)) ((y) (set! y val))
            ((t theta) (set! theta val))
            ((vx) (set! vx val)) ((vy) (set! vy val))
            ((o omega) (set! omega val))
            ((x1) (set! x1 val)) ((y1) (set! y1 val))
            ((x2) (set! x2 val)) ((y2) (set! y2 val)))) ))))

(define (display-dominos dominos)
  (display "[")
  (for-each
    (lambda (d)
      (let ((x0 (d 'x))
            (y0 (d 'y))
            (theta (d 'theta)))
      (let ((dx (* *height/2* (cos theta)))
            (dy (* *height/2* (sin theta))))
      (print (string-append
                  (number->string (+ x0 dx))
                  ","
                  (number->string (+ y0 dy))
                  ","
                  (number->string (- x0 dx))
                  ","
                  (number->string (- y0 dy))
                  ",")))))
    dominos)
  (display "],"))

(define (read-dominos)
  (let loop ((ac '()))
    (let1 a (read)
    (if (eq? a 'EOF) ac
        (loop (cons (make-domino a (read) (read)) ac))))))

