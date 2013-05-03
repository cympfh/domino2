;; of output
(define *skip-frame* 30)
(define *max-frame* 10000)

;; of domino
(define *height* 50)
(define *height/2* (/ *height* 2))
(define *M* 10)
(define *I* (* *M* *height* *height* (/ 12)))
(define */I* (/ *I*))

;; of world
(define *dt* 0.001)
(define *g* 9.8)
(define *-g* (- *g*))

(define (update d rest)
  (for-each
    (lambda (e)
      (when (intersect d e)
            (collision d e)))
    rest)
  (fall d))

(define (collision d e)
  (let1 dx (/ (- (+ (d 'x1) (d 'x2)) (+ (e 'x1) (e 'x2))) 20)
    (begin (inc! e 'vx (- dx)) 
           (inc! d 'vx dx)))
  (let1 o (/ (+ (d 'o) (e 'o)) 2)
  (d 'o o)
  (e 'o o)))

(define (fall d)
  (call/cc (lambda (return)
    (inc! d 'vy *-g*) ;; gravity

    (let1 dx (* (d 'vx) *dt*)
      (inc! d 'x1 (* (d 'vx) *dt*))
      (inc! d 'x2 (* (d 'vx) *dt*)))

    (let1 dy (* (d 'vy) *dt*)
      (inc! d 'y1 (* (d 'vy) *dt*))
      (inc! d 'y2 (* (d 'vy) *dt*)))

    (inc! d 'theta (* (d 'omega) *dt*))
    ; (format #t "/* omega = ~a */ \n" (d 'omega))

    ; floor check
    (cond
      ((and (< (d 'y1) 0) (< (d 'y2) 0))
        ;(format #t
        ;  "/* y = ~a, ~a\n  vy = ~a  theta = ~a ; omega = ~a */\n"
        ;  (d 'y1) (d 'y2) (d 'vy) (d 't) (d 'o))
        (return #f))

      ((<= (d 'y1) 0)
        (d 'y1 0)
        (d 'vy 0)
        (inc! d 'omega
          (* -1 *M* *g* (cos (d 't)) *height/2* */I* *dt*))
        (let ((c (cos (d 't))) (s (sin (d 't))))
        (d 'x2 (+ (d 'x1) (* *height* c)))
        (d 'y2 (* *height* s))))

      ((<= (d 'y2) 0)
        (d 'y2 0)
        (d 'vy 0)
        (inc! d 'omega
          (* *M* *g* (cos (d 't)) *height/2* */I* *dt*))
        (let ((c (cos (d 't))) (s (sin (d 't))))
        (d 'x2 (- (d 'x2) (* *height* c)))
        (d 'y2 (* -1 *height* s)))))

    (return #t))))

(define (update-dominos ds)
  (if (null? ds) '()
      (if (update (car ds) (cdr ds))
          (cons (car ds) (update-dominos (cdr ds)))
          (update-dominos (cdr ds)))))

;; main
(define *cx* 0)
(begin
  (print "data=[")
  (let loop ((ds (read-dominos)))
    (set! *cx* (+ *cx* 1))
    (if (and (< *cx* *max-frame*) (not (null? ds)))
      (let1 ds (update-dominos ds)
      (if (zero? (modulo *cx* *skip-frame*))
        (display-dominos ds))
      (loop ds))))
  (print "]"))
