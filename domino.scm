(define *height* 50)
(define *height/2* (/ *height* 2))

(define *dt* 0.01)

(define (update d)
  (call/cc (lambda (return)
    (inc! d 'vy -2) ;; gravity

    (let1 dx (* (d 'vx) *dt*)
      (inc! d 'x  (* (d 'vx) *dt*))
      (inc! d 'x1 (* (d 'vx) *dt*))
      (inc! d 'x2 (* (d 'vx) *dt*)))

    (let1 dy (* (d 'vy) *dt*)
      (inc! d 'y  (* (d 'vy) *dt*))
      (inc! d 'y1 (* (d 'vy) *dt*))
      (inc! d 'y2 (* (d 'vy) *dt*)))

    ; floor check
    (cond
      ((and (< (d 'y1) 0) (< (d 'y2) 0)) (return #f))
      ((< (d 'y1) 0)
        (let1 dy (- (d 'y1))
          (inc! d 'y  dy)
          (inc! d 'y1 dy)
          (inc! d 'y2 dy)))
      ((< (d 'y2) 0)
        (let2 dy (- (d 'y2))
          (inc! d 'y  dy)
          (inc! d 'y1 dy)
          (inc! d 'y2 dy))))
    (inc! d 'theta (* (d 'omega) *dt*))

    (return #t))))

(define (update-dominos ds)
  (if (null? ds) '()
      (if (update (car ds))
          (cons (car ds) (update-dominos (cdr ds)))
          (update-dominos (cdr ds)))))

;; main
(define *cx* 0)
(begin
  (print "data=[")
  (let loop ((ds (read-dominos)))
    (set! *cx* (+ *cx* 1))
    (if (and (< *cx* 1000) (not (null? ds)))
      (let1 ds (update-dominos ds)
      (display-dominos ds)
      (loop ds))))
  (print "]"))
