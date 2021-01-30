(define a '(1 2 3 4 5 6))

(define b (map (lambda (x) (+ x 1)) a))

(displayln b)
