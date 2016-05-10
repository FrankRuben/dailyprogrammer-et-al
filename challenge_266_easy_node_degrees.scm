;; For Chez Scheme

(define-syntax define-with-bit-vectors
  (syntax-rules ()
    ((_ (fun-name msg idx-var len-var item-var) loop-body ...)
     (define (fun-name v)
       (format #t "~a (~d):~%" msg (fxvector-length v))
       (do ((i 0 (+ i 1))
            (l (fxvector-length v) l))
           ((= i l))
         ((lambda (idx-var len-var item-var)
            loop-body ...) i l (fxvector-ref v i)))
       (newline)))))

(define-with-bit-vectors (print-degree "Degrees" idx len item)
  (format #t "Node ~d has a degree of ~d~%" (+ idx 1) (bitwise-bit-count item)))

(define (print-node n len)
  (do ((i 0 (+ i 1)))
      ((= i len))
    (format #t "~d " (if (bitwise-bit-set? n i) 1 0))))

(define-with-bit-vectors (print-adjacency "Adjacency matrix" idx len item)
  (print-node item len)
  (newline))

(define (process lines)
  (let* ((adjacency-set! (lambda (v n1 n2)
                          (let* ((c (fxvector-ref v (- n1 1)))
                                 (n (bitwise-copy-bit c (- n2 1) 1)))
                            (fxvector-set! v (- n1 1) n))))
         (p (open-input-string lines))
         (n (read p))
         (v (make-fxvector n 0)))
    (let loop ((n1 (read p)))
      (if (eof-object? n1)
          (begin
            (print-degree v)
            (print-adjacency v))
          (let ((n2 (read p)))
            (adjacency-set! v n1 n2)
            (adjacency-set! v n2 n1)
            (loop (read p)))))))

(process "3
1 2
1 3")

(process "16
1 2
1 3
2 3
1 4
3 4
1 5
2 5
1 6
2 6
3 6
3 7
5 7
6 7
3 8
4 8
6 8
7 8
2 9
5 9
6 9
2 10
9 10
6 11
7 11
8 11
9 11
10 11
1 12
6 12
7 12
8 12
11 12
6 13
7 13
9 13
10 13
11 13
5 14
8 14
12 14
13 14
1 15
2 15
5 15
9 15
10 15
11 15
12 15
13 15
1 16
2 16
5 16
6 16
11 16
12 16
13 16
14 16
15 16")


;; for comparison, the basic processing in CL:
;; (defun process (lines)
;;   (with-input-from-string (input lines)
;;     (let* ((n (read input))
;;            (v (make-array `(,n ,n) :element-type 'bit)))
;;       (loop for n1 = (read input nil)
;;          while n1
;;          for n2 = (read input nil)
;;          do (setf (sbit v (1- n1) (1- n2)) 1
;;                   (sbit v (1- n2) (1- n1)) 1))
;;       (format t "~a~%" v))))


;; See https://www.reddit.com/r/dailyprogrammer/comments/4ijtrt/20160509_challenge_266_easy_basic_graph/
