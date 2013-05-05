;; of output
(define *skip-frame* 30)
(define *max-frame* 100000)

;; of domino
(define *height* 50)
(define *height/2* (/ *height* 2))
(define *width* 5)
(define *width/2* (/ *width* 2))
(define *arg* (atan (/ *width* *height*)))
(define *M* 10)
(define *I* (* *M* *height* *height* (/ 12)))
(define */I* (/ *I*))

;; of world
(define *dt* 0.001)
(define *g* 9.8)
(define *Mg* (* *M* *g*))
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
  (let1 P (/ (- (+ (d 'x1) (d 'x2)) (+ (e 'x1) (e 'x2))) 2)
  (receive (x y) (find-intersect d e)
  (coll! d P x y)
  (coll! e P x y))))

(define (fall d)
  (define (frict!)
    (let1 f (* 0.4 *g*)
    (if (> (d 'vx) 0)
        (begin (inc! d 'vx (- f))
               (when (< (d 'vx) 0) (d 'vx 0)))
        (begin (inc! d 'vx f)
               (when (> (d 'vx) 0) (d 'vx 0))))))

  (define (moment theta)
     (cond ((> theta (+ *pi/2* *arg*))
              (* *Mg* (cos (- theta *arg*)) *height/2*))
           ((> theta *pi/2*)
              (* *Mg* (cos (- theta *arg*)) *width/2*))
           ((< theta (- *pi/2* *arg*))
              (* *Mg* (cos (+ theta *arg*)) *height/2*))
           (else
              (* *Mg* (cos (+ theta *arg*)) *width/2*))))

  (call/cc (lambda (return)
    (inc! d 'vy *-g*) ;; gravity

    (let1 dx (* (d 'vx) *dt*)
      (inc! d 'x1 (* (d 'vx) *dt*))
      (inc! d 'x2 (* (d 'vx) *dt*)))

    (let1 dy (* (d 'vy) *dt*)
      (inc! d 'y1 (* (d 'vy) *dt*))
      (inc! d 'y2 (* (d 'vy) *dt*)))

    (inc! d 'theta (* (d 'omega) *dt*))

    ; floor check
    (cond
      ((and (< (d 'y1) 0) (< (d 'y2) 0)) (return #f))

      ((<= (d 'y1) 0)
        (d 'y1 0)
        (d 'vy 0)
        (frict!)
        (let* ((theta (d 't))
               (c (cos theta)) (s (sin theta))
               (N (moment theta)))
        (inc! d 'omega (* -1 N */I* *dt*))
        (d 'x2 (+ (d 'x1) (* *height* c)))
        (d 'y2 (* *height* s))))

      ((<= (d 'y2) 0)
        (d 'y2 0)
        (d 'vy 0)
        (frict!)
        (let* ((theta (d 't))
               (c (cos theta)) (s (sin theta))
               (N (moment theta)))
        (inc! d 'omega (* N */I* *dt*))
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
