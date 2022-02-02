
#lang racket
 
(require racket/draw)
(require racket/gui/base)


(define world
    '( (0 0 2 0)
       (2 0 2 0)
       (0 0 0 0)
       (0 0 8 0) ))


(define (fill-zeros n)
    (cond
        [(<= n 0)
            '()]
        [(= n 1)
            (if (< (random) 0.1) '(2) '(0))]
        [else
            (cons 0 (fill-zeros (- n 1)))]))


(define (collapse-row row)
    (cond
        [(eq? row '())
            '()]
        [(eq? (car row) 0)
            (collapse-row (cdr row))]
        [(eq? (cdr row) '())
            row]
        [(eq? (car (cdr row)) 0)
            (collapse-row (cons (car row) (cdr (cdr row))))]
        [(eq? (car row) (car (cdr row)))
            (cons (* (car row) 2) (collapse-row (cdr (cdr row))))]
        [else
            (cons (car row) (collapse-row (cdr row)))]))


(define (to-columns rows)
    (map (lambda (n)
        (map (lambda (row)
            (list-ref row n))
            rows))
        (stream->list (in-range 0 4))))


(define (shift-world-up world)
    (to-columns (map (lambda (col)
        (let ([new-column  (collapse-row col)])
            (append new-column (fill-zeros (- 4 (length new-column))))))
        (to-columns world))))


(define (shift-world-down world)
    (to-columns (map (lambda (col)
        (let ([new-column  (collapse-row col)])
            (append (reverse (fill-zeros (- 4 (length new-column)))) new-column)))
        (to-columns world))))


(define (shift-world-left world)
    (map (lambda (row)
        (let ([new-row  (collapse-row row)])
            (append new-row (fill-zeros (- 4 (length new-row))))))
        world))


(define (shift-world-right world)
    (map (lambda (row)
        (let ([new-row  (collapse-row row)])
            (append (reverse (fill-zeros (- 4 (length new-row)))) new-row)))
        world))


(define (render-square dc x y num)
    (send dc set-scale 3 3)
    (send dc draw-rectangle (+ 5 (* x 40)) (+ 5 (* y 40)) 30 30)
    (send dc draw-text (format "~a" num) (+ 15 (* x 40)) (+ 10 (* y 40))))


(define (render-world dc world)
    (for
        ([row world]
         [y (in-range 0 4)])
            (for
                ([num row]
                 [x (in-range 0 4)])
                    (render-square dc x y num))))


(define frame (new frame% [label "Racket2048"]))


(define event-canvas%
    (class canvas%
        (define/override (on-char event)
            (let ([key  (send event get-key-code)])
                (cond
                    [(eq? key 'up)      (set! world (shift-world-up world))]
                    [(eq? key 'down)    (set! world (shift-world-down world))]
                    [(eq? key 'left)    (set! world (shift-world-left world))]
                    [(eq? key 'right)   (set! world (shift-world-right world))])
                (send this refresh)))
        (super-new)))


(new event-canvas% [parent frame]
    [paint-callback
        (lambda (canvas dc)
            (render-world dc world))])

(send frame show #t)

