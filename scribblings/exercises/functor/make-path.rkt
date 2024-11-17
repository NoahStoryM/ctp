#lang racket/base

(require racket/match)
(provide make-path)

(define (make-path 𝒢 n str)
  (for/fold ([g* (list (vector-immutable #\nul n n))]
             [n0 n]
             #:result g*)
            ([ch (in-list (string->list str))])
    (let/cc return
      (for ([g (in-vector 𝒢)])
        (match g
          [`#[,a ,n ,n1]
           #:when (and (char=? a ch) (equal? n n0))
           (return (cons g g*) n1)]
          [_ (void)]))
      (raise-arguments-error
       'make-path "invalid string or node"
       "string" str
       "node" n))))
