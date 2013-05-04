(define-syntax inc!
  (syntax-rules ()
    ((_ d sym diff) (d sym (+ (d sym) diff)))))

(define-syntax let1
  (syntax-rules ()
    ((_ a x body ...) (let ((a x)) body ...))))

(define-syntax when
  (syntax-rules ()
    ((_ test body ...) (if test (begin body ...)))))

(define-syntax cross
  (syntax-rules ()
    ((_ x1 y1 x2 y2) (- (* x1 y2) (* x2 y1)))))

(define-syntax ccw
  (syntax-rules ()
    ((_ x1 y1 x2 y2 x0 y0)
     (if (<= 0 (cross (- x1 x0) (- y1 y0) (- x2 x0) (- y2 y0)))
         1 -1))))

(define-syntax kado
  (syntax-rules ()
    ((_ th)
     (let1 alpha 0.01
     (if (<= th *pi/2*) (+ th alpha) (- th alpha))))))
