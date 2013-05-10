;; of output
(define *skip-frame* 80)
(define *max-frame* 100000)

;; of domino
(define *height* 50)
(define *height/2* (/ *height* 2))
(define *width* 5)
(define *width/2* (/ *width* 2))
(define *arg* (atan (/ *width* *height*)))
(define *M* 10)
(define */M* (/ *M*))
(define *I* (* *M* *height* *height* (/ 12)))
(define */I* (/ *I*))

;; of world
(define *dt* 0.0005)
(define *g* 9.8)
(define *Mg* (* *M* *g*))
(define *-g* (- *g*))
(define *pi* 3.1415926535897932384626434)
(define *pi/2* (/ *pi* 2))

(define (update d rest)
  (for-each
    (lambda (e)
      (when (intersect d e)
            (collision! d e)))
    rest)
  (fall d))

(define (velocity-at d x y)
  (map + (list (d 'vx) (d 'vy))
         (cond ((< (d 'y1) 1)
                     (map (lambda (f) (* f (d 'o) (distance x y (d 'x1) (d 'y1))))
                          (list (- (sin (d 't))) (cos (d 't)))))
               ((< (d 'y2) 1)
                     (map (lambda (f) (* f (d 'o) (distance x y (d 'x2) (d 'y2))))
                          (list (sin (d 't)) (- (cos (d 't))))))
               (else '(0 0)))))

(define (collision! d e)
  (define (dot a b) (apply + (map * a b)))
  (define (away! d e)
    (receive (dx _) (foot d)
    (receive (ex _) (foot e)
    (let while ()
      (when (intersect d e)
            (if (< dx ex)
                (begin (inc! d 'x1 -0.1)
                       (inc! d 'x2 -0.1)
                       (inc! e 'x1 0.1)
                       (inc! e 'x2 0.1))
                (begin (inc! d 'x1 0.1)
                       (inc! d 'x2 0.1)
                       (inc! e 'x1 -0.1)
                       (inc! e 'x2 -0.1)))
            (while))))))

  (receive (x y) (find-intersect d e)
  (let ((dx (map - (if (> (d 'y1) (d 'y2)) (list (d 'x1) (d 'y1))
                                           (list (d 'x2) (d 'y2)))
                   (if (> (e 'y1) (e 'y2)) (list (e 'x1) (e 'y1))
                                           (list (e 'x2) (e 'y2)))))
        (vr (map - (velocity-at d x y) (velocity-at e x y))))
  (let1 A (* -480 (/ (dot dx vr) (dot vr vr)))
  (let ((Px (* A (car vr))) (Py (* A (cadr vr))))
  (inc! d 'vx (* Px *dt* */M*))
  (inc! d 'vy (* Py *dt* */M*))
  (inc! e 'vx (* Px *dt* */M* -1))
  (inc! e 'vy (* Py *dt* */M* -1))
  (receive (fx fy) (center-of-mass d)
    (inc! d 'omega (* */I* *dt* (cross (- x fx) (- y fy) Px Py))))
  (receive (fx fy) (center-of-mass e)
    (inc! e 'omega (* */I* *dt* (cross (- x fx) (- y fy) Px Py) -1)))
  (away! d e)
  )))))

(define (fall d)
  (define (frict!)
    (let1 f (* 0.4 *g*)
    (when (> (d 'vx) 0)
          (begin (inc! d 'vx (- f))
                 (when (< (d 'vx) 0) (d 'vx 0))))
    (when (< (d 'vx) 0)
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
    (when (< (d 't) 0) (d 't 0))
    (when (> (d 't) *pi*) (d 't *pi*))

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
