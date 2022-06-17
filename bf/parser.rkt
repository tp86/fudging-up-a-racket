#lang racket

(provide parse-expr)

;; parse-expr: any input-port -> (U syntax eof)
;; Either produces a syntax object or the eof object
(define (parse-expr src in)
  (let*-values ([(line column position) (port-next-location in)]
                [(next-char) (read-char in)]
                [(decorate) (λ (sexp span)
                              (datum->syntax #f sexp (list src line column position span)))]);)))])))
    (cond
      [(eof-object? next-char) eof]
      [else
        (case next-char
          [(#\<) (decorate '(less-than) 1)]
          [(#\>) (decorate '(greater-than) 1)]
          [(#\+) (decorate '(plus) 1)]
          [(#\-) (decorate '(minus) 1)]
          [(#\,) (decorate '(comma) 1)]
          [(#\.) (decorate '(period)1)]
          [(#\[)
           ;; The slightly messy case is bracket.  We keep reading
           ;; a list of exprs, and then construct a wrapping bracket
           ;; around the whole thing.
           (let-values ([(elements) (parse-exprs src in)]
                        [(l c tail-position) (port-next-location in)])
             (decorate `(brackets ,@elements) (- tail-position position)))]
          [else (parse-expr src in)])])))

;; parse-exprs: input port -> (listof syntax)
;; Parse a list of expressions
(define (parse-exprs source-name in)
  (let ([peeked-char (peek-char in)])
    (cond
      [(eof-object? peeked-char)
       (error 'parse-exprs "Expected ], but read eof")]
      [(char=? peeked-char #\])
       (read-char in)
       empty]
      [(member peeked-char (list #\< #\> #\+ #\- #\, #\. #\[))
       (cons (parse-expr source-name in)
             (parse-exprs source-name in))]
      [else
        (read-char in)
        (parse-exprs source-name in)])))
