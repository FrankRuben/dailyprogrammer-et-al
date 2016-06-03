#lang typed/racket

(require (only-in srfi/8 receive))

(: make-indent (->* (Fixnum) (String) String))
(define (make-indent n [spaces "    "])
  (string-append* (make-list n spaces)))

(: statement-tos (-> (Listof Symbol) (Option Symbol)))
(define (statement-tos stmt-stack)
  (if (pair? stmt-stack) (car stmt-stack) #f))

(: handle-statement (-> (Listof Symbol) String (Values (Listof Symbol) Fixnum (Option String))))
(define (handle-statement stmt-stack line)
  (let ((tos : Symbol (or (statement-tos stmt-stack) 'EMPTY))
        (stmt : Symbol (let ((p (string-split line)))
                         (if (pair? p) (string->symbol (car p)) 'NONE))))
    (match (cons tos stmt)
      [(cons _ 'IF)
       (values (cons 'IF stmt-stack) (length stmt-stack) #f)]
      [(cons _ 'FOR)
       (values (cons 'FOR stmt-stack) (length stmt-stack) #f)]
      [(cons 'IF 'ENDIF)
       (values (cdr stmt-stack) (- (length stmt-stack) 1) #f)]
      [(cons _ 'ENDIF)
       (values stmt-stack (length stmt-stack) (format "Found ~a following ~a" stmt tos))]
      [(cons 'FOR 'NEXT)
       (values (cdr stmt-stack) (- (length stmt-stack) 1) #f)]
      [(cons _ 'NEXT)
       (values stmt-stack (length stmt-stack) (format "Found ~a following ~a" stmt tos))]
      [_
       (values stmt-stack (length stmt-stack) #f)])))

(: format-line (-> (Listof Symbol) String (Values (Listof Symbol) String String (Option String))))
(define (format-line stmt-stack line)
  (let ((trimmed-line (string-trim line #px"[·»]+" #:right? #f)))
    (receive (new-stmt-stack indent-level opt-error)
        (handle-statement stmt-stack trimmed-line)
      (if opt-error
          (values new-stmt-stack "" trimmed-line opt-error)
          (values new-stmt-stack (make-indent indent-level) trimmed-line #f)))))

(: run-formatter (-> String Void))
(define (run-formatter lines)
  (let* ((p : Input-Port (open-input-string lines))
         (n : Fixnum (assert (read p) fixnum?)))
    (let loop ((stmt-stack : (Listof Symbol) '())
               (line : (U String EOF) (read-line p)))
      (if (eof-object? line)
          (let ((tos (statement-tos stmt-stack)))
            (when tos
              (printf "ERROR: Dangling ~a~%" tos)))
          (receive (new-stmt-stack indent formatted-line opt-error)
              (format-line stmt-stack line)
            (if opt-error
                (printf "~a <-- ERROR: ~a~%" formatted-line opt-error)
                (begin
                  (printf "~a~a~%" indent formatted-line)
                  (loop new-stmt-stack (read-line p)))))))))

(module+ main
  (run-formatter
   #<<~~eof
12
····
VAR I
·FOR I=1 TO 31
»»»»IF !(I MOD 3) THEN
··PRINT "FIZZ"
··»»ENDIF
»»»»····IF !(I MOD 5) THEN
»»»»··PRINT "BUZZ"
··»»»»»»ENDIF
»»»»IF (I MOD 3) && (I MOD 5) THEN
······PRINT "FIZZBUZZ"
··»»ENDIF
»»»»·NEXT
~~eof
   )

  (run-formatter
   #<<~~eof
4
FOR I=0 TO 10
····IF I MOD 2 THEN
········PRINT I
NEXT
~~eof
   )

  (run-formatter
   #<<~~eof
4
FOR I=0 TO 10
····IF I MOD 2 THEN
········PRINT I
~~eof
   )

  (run-formatter
   #<<~~eof
3
FOR I=0 TO 10
····PRINT I
ENDIF
~~eof
   ))

;;; See: https://www.reddit.com/r/dailyprogrammer/comments/4lpygb/20160530_challenge_269_easy_basic_formatting/
;;; Output:

;; VAR I
;; FOR I=1 TO 31
;;     IF !(I MOD 3) THEN
;;         PRINT "FIZZ"
;;     ENDIF
;;     IF !(I MOD 5) THEN
;;         PRINT "BUZZ"
;;     ENDIF
;;     IF (I MOD 3) && (I MOD 5) THEN
;;         PRINT "FIZZBUZZ"
;;     ENDIF
;; NEXT
;;
;; FOR I=0 TO 10
;;     IF I MOD 2 THEN
;;         PRINT I
;; NEXT <-- ERROR: Found NEXT following IF
;;
;; FOR I=0 TO 10
;;     IF I MOD 2 THEN
;;         PRINT I
;; ERROR: Dangling IF
;;
;; FOR I=0 TO 10
;;     PRINT I
;; ENDIF <-- ERROR: Found ENDIF following FOR

;;; Local Variables:
;;; compile-command: "racket dp269-basic-easy.rkt"
;;; End:
