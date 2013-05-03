(define *height* 50)
(define *height/2* (/ *height* 2))

(define (update d)
  (inc! d 'y -2)
  (and (> (d 'y) 0) #t))

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
  (print "]\n"))
