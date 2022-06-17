#lang racket

(require rackunit)
(provide (all-defined-out))

;; State structure containing byte array of data and pointer to data
(struct state (data
               ptr)
  #:mutable)

;; Creates a new state, with a byte array of 30000 zeros, and the pointer at
;; index 0.
(define (new-state)
  (state (make-vector 30000 0)
         0))

;; Increment the data pointer
(define (increment-ptr a-state)
  (set-state-ptr! a-state (add1 (state-ptr a-state))))

;; Decrement the data pointer
(define (decrement-ptr a-state)
  (set-state-ptr! a-state (sub1 (state-ptr a-state))))

;; Increment the byte at the data pointer
(define (increment-byte a-state)
  (let* ([v (state-data a-state)]
         [i (state-ptr a-state)]
         [d (vector-ref v i)])
    (vector-set! v i (add1 d))))

;; Decrement the byte at the data pointer
(define (decrement-byte a-state)
  (let* ([v (state-data a-state)]
         [i (state-ptr a-state)]
         [d (vector-ref v i)])
    (vector-set! v i (sub1 d))))

;; Print the byte at the data pointer
(define (write-byte-to-stdout a-state)
  (let* ([v (state-data a-state)]
         [i (state-ptr a-state)]
         [d (vector-ref v i)])
    (write-byte d (current-output-port))))

;; Read a byte from stdin into the data pointer
(define (read-byte-from-stdin a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (read-byte (current-input-port)))))

;; Loops
(define-syntax-rule (loop a-state body ...)
  (local [(define (loop)
            (unless (= (vector-ref (state-data a-state) (state-ptr a-state))
                       0)
              body ...
              (loop)))]
    (loop)))

;; Simple examples
(let ([s (new-state)])
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (increment-byte s)
  (check-equal? 2 (vector-ref (state-data s) 0))
  (decrement-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0)))

;; Pointer movement
(let ([s (new-state)])
  (increment-ptr s)
  (increment-byte s)
  (check-equal? 0 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (decrement-ptr s)
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1)))

;; Make sure stdin is doing something
(let ([s (new-state)])
  (parameterize ([current-input-port (open-input-bytes (bytes 3 1 4))])
    (read-byte-from-stdin s)
    (increment-ptr s)
    (read-byte-from-stdin s)
    (increment-ptr s)
    (read-byte-from-stdin s))
  (check-equal? 3 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (check-equal? 4 (vector-ref (state-data s) 2)))

;; Make sure stdout is doing something
(let ([s (new-state)])
  (set-state-data! s (vector 80 76 84))
  (let ([simulated-stdout (open-output-string)])
    (parameterize ([current-output-port simulated-stdout])
      (write-byte-to-stdout s)
      (increment-ptr s)
      (write-byte-to-stdout s)
      (increment-ptr s)
      (write-byte-to-stdout s))
    (check-equal? "PLT" (get-output-string simulated-stdout))))

;; Let's see that we can clear using loops
(let ([s (new-state)])
  (set-state-data! s (vector 0 104 101 108 112 109 101 105
                             109 109 101 108 116 105 110 103))
  (set-state-ptr! s 15)
  ;; [ [-] < ]
  (loop s
        (loop s (decrement-byte s))
        (decrement-ptr s))
  (check-equal? 0 (state-ptr s))
  (check-equal? (make-vector 16 0) (state-data s)))
