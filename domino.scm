;; of output
(define *skip-frame* 30)
(define *max-frame* 100000)

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
(define *pi* 3.1415926535897932384626434)
(define *pi/2* (/ *pi* 2))

(define (update d rest)
  (for-each
    (lambda (e)
      (when (intersect d e)
            (collision d e)))
    rest)
  (fall d))

(define (collision d e)
  (find-intersect d e)
  (let1 dx (/ (- (+ (d 'x1) (d 'x2)) (+ (e 'x1) (e 'x2))) 2)
    (begin (inc! e 'vx (- dx)) 
           (inc! d 'vx dx)))
  (let1 o (/ (+ (d 'o) (e 'o)) 2)
  (d 'o o)
  (e 'o o)))

(define (fall d)
  (define (frict!)
    (d 'vy (* (d 'vy) 0.93))
    (d 'vx (* (d 'vx) 0.8)))
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
        (frict!)
        (let* ((th (kado (d 't)))
               (c (cos th)) (s (sin th)))
        (inc! d 'omega
          (* -1 *M* *g* c *height/2* */I* *dt*))
        (d 'x2 (+ (d 'x1) (* *height* c)))
        (d 'y2 (* *height* s))))

      ((<= (d 'y2) 0)
        (d 'y2 0)
        (d 'vy 0)
        (frict!)
        (let* ((th (kado (d 't)))
               (c (cos th)) (s (sin th)))
        (inc! d 'omega
          (* *M* *g* c *height/2* */I* *dt*))
        (d 'x2 (- (d 'x2) (* *height* c)))
        (d 'y2 (* -1 *height* s)))))

    (return #t))))

(define (update-dominos ds zs)
  (let rec ((ls ds) (ds '()) (zs zs))
    (if (null? ls) (values ds zs)
        (if (update (car ls) (cdr ls))
            (rec (cdr ls) (cons (car ls) ds) zs)
            (rec (cdr ls) ds (cons (car ls) zs))))))

;; main
(define *cx* 0)
(begin
  (print "data=[")
  (let loop ((ds (read-dominos)) (zombies '()))
    (set! *cx* (+ *cx* 1))
    (when (and (< *cx* *max-frame*) (not (null? ds)))
          (receive (new-ds new-zs) (update-dominos ds zombies)
            (when (zero? (modulo *cx* *skip-frame*))
                  (display-dominos (append new-ds new-zs)))
            (loop new-ds new-zs))))
  (print "]"))
