(module dp331 (main main))

(define *rg-lexer*
  (regular-grammar ((fun-decl "fun")    ;Bigloo's parser is LALR(1), so we need to help him a bit, so that...
                    (var-decl "var")    ;...he doesn't have to look ahead too far.
                    (ident (: (or #\_ alpha) (* (or #\_ alnum))))
                    (number (: (? (or #\+ #\-)) (+ digit) (? (: "." (+ digit)))))
                    (comma #\,)
                    (whitespace (+ (or #\tab #\space)))
                    (newline (: (? #\return) #\newline)))
                   (fun-decl 'fun-decl)
                   (var-decl 'var-decl)
                   (ident (cons 'ident (the-string)))
                   (number (cons 'number (the-flonum))) ;`the-flonum' also handles the optional sign
                   (comma 'comma)
                   (whitespace (ignore))
                   (newline 'newline)
                   (#\= 'op-assign) (#\( 'op-lparen) (#\) 'op-rparen)
                   (#\+ 'op-add) (#\- 'op-sub) (#\* 'op-mult) (#\/ 'op-div) (#\^ 'op-exp)))

(define (eval-expr expr var-scope fun-scope)

  (define (hash-table-copy ht add-size::int)
    (let ((new-ht (create-hashtable :size (+ (hashtable-size ht) add-size))))
      (hashtable-for-each ht (lambda (key value) (hashtable-put! new-ht key value)))
      new-ht))

  (define (make-var-scope ident::symbol formals actuals)
    (if (or formals actuals)
        (if (= (if formals (length formals) 0) (if actuals (length actuals) 0))
            (let ((new-ht (hash-table-copy var-scope (length formals))))
              (map (lambda (formal actual)
                     (hashtable-put! new-ht (string->symbol formal) actual))
                   formals actuals)
              new-ht)
            (error "parser" "incorrect actuals for function call" ident))
        var-scope))

  (cond
   ((number? expr)
    expr)
   ((symbol? expr)
    (let ((value (hashtable-get var-scope expr)))
      (or value (error "parser" "unknown variable identifier" expr))))
   ((list? expr)
    (if (eq? (car expr) 'funcall)
        (let* ((fun-ident (cadr expr))
               (fun-def (hashtable-get fun-scope fun-ident)))
          (if fun-def
              (let* ((fun-formals (car fun-def)) ;we stored the function's formals and its ast during definition and...
                     (fun-ast (cdr fun-def))
                     (fun-actuals (caddr expr))  ;...we passed the actuals from the function call
                     (evaled-actuals (if fun-actuals
                                         (map (lambda (actual)
                                                (eval-expr actual var-scope fun-scope))
                                              fun-actuals)
                                         fun-actuals)))
                (eval-expr fun-ast (make-var-scope fun-ident fun-formals evaled-actuals) fun-scope))
              (error "parser" "unknown function identifier" expr)))
        (let ((a (eval-expr (cadr expr)  var-scope fun-scope))
              (b (eval-expr (caddr expr) var-scope fun-scope)))
          (case (car expr)
            ('expt (expt a b))
            ('mult (*    a b))
            ('div  (/    a b))
            ('add  (+    a b))
            ('sub  (-    a b))))))))

(define (make-lalr-grammar var-scope fun-scope)
  (lalr-grammar
   ((right: op-exp)                     ; terminal element definitions
    (left: op-mult op-div)
    (left: op-add op-sub)
    fun-decl var-decl
    op-lparen op-rparen
    ident number newline comma op-assign)
   (lines                               ; non-terminal element definitions
    (())
    ((lines stmt) stmt))
   (stmt
    ((expr newline) expr)
    ((var-decl assign-var newline) assign-var)
    ((fun-decl assign-fun newline) assign-fun))
   (assign-var
    ((ident op-assign expr)
     (let ((value (eval-expr expr var-scope fun-scope)))
       (hashtable-put! var-scope (string->symbol ident) value)
       value)))
   (assign-fun
    ((ident op-lparen formals op-rparen op-assign expr)
     (hashtable-put! fun-scope (string->symbol ident) (cons formals expr))
     ident))
   (formals
    (())
    ((ident) (list ident))
    ((ident comma formals) (cons ident formals)))
   (expr
    ((number) number)
    ((ident) (string->symbol ident))
    ((ident op-lparen actuals op-rparen) `(funcall ,(string->symbol ident) ,actuals))
    ((expr@a op-exp expr@b)  `(expt ,a ,b))
    ((expr@a op-mult expr@b) `(mult ,a ,b))
    ((expr@a op-div expr@b)  `(div  ,a ,b))
    ((expr@a op-add expr@b)  `(add  ,a ,b))
    ((expr@a op-sub expr@b)  `(sub  ,a ,b))
    ((op-lparen expr op-rparen) expr))
   (actuals
    (())
    ((expr) (list expr))
    ((expr comma actuals) (cons expr actuals)))))

(define (main argv)

  (define (parse-expr expr-line::string lalr-grammar var-scope fun-scope)
    (with-input-from-string expr-line
      (lambda ()
        (eval-expr
         (read/lalrp lalr-grammar *rg-lexer* (current-input-port))
         var-scope fun-scope))))

  (define (eval-print-expr expr-str::string expected grammar var-scope fun-scope)
    (printf "~a => ~a (expected: ~a)~%"
            expr-str
            (parse-expr (format "~a~%" expr-str) grammar var-scope fun-scope)
            expected))

  (let* ((var-scope (make-hashtable))
         (fun-scope (make-hashtable))
         (grammar (make-lalr-grammar var-scope fun-scope)))
    (for-each (lambda (tag::string)
                (cond
                 ((string=? tag "sample")
                  (eval-print-expr "5 + 5" 10 grammar var-scope fun-scope)
                  (eval-print-expr "(2 * 5 + 1) / 10" 1.1 grammar var-scope fun-scope)
                  (eval-print-expr "var x =  1 / 2" 0.5 grammar var-scope fun-scope)
                  (eval-print-expr "var y = x * 2" 1 grammar var-scope fun-scope))
                 ((string=? tag "challenge")
                  (eval-print-expr "9 + 10" 19 grammar var-scope fun-scope)
                  (eval-print-expr "(2 * 5 + 1) / 10" 1.1 grammar var-scope fun-scope)
                  (eval-print-expr "var x =  1 / 2" 0.5 grammar var-scope fun-scope)
                  (eval-print-expr "var y = x * 2" 1 grammar var-scope fun-scope)
                  (eval-print-expr "(x + 2) * (y * (5 - 100))" -237.5 grammar var-scope fun-scope)
                  (eval-print-expr "var z = 5*-3.14" -15.7 grammar var-scope fun-scope)
                  (eval-print-expr "2.6^(2 + 3/2) * (2-z)" 501.625937332 grammar var-scope fun-scope))
                 ((string=? tag "bonus")
                  (eval-print-expr "var a = 10" 10.0 grammar var-scope fun-scope)
                  (eval-print-expr "fun a() = 2" #f grammar var-scope fun-scope)
                  (eval-print-expr "a() + a" 12.0 grammar var-scope fun-scope)
                  (eval-print-expr "fun avg(a, b) = (a + b) / 2" #f grammar var-scope fun-scope)
                  (eval-print-expr "var x = avg(69, 96)" 82.5 grammar var-scope fun-scope)
                  (eval-print-expr "avg(x, avg(a(), a)) + a" 54.25 grammar var-scope fun-scope))))
              '("sample" "challenge" "bonus"))))

;; See: https://www.reddit.com/r/dailyprogrammer/comments/7096nu/20170915_challenge_331_hard_interactive/
;; Run with: bigloo dp331-hard-interpreter.scm -o dp331-hard-interpreter && ./dp331-hard-interpreter sample

;; Output:
;;   5 + 5 => 10.0 (expected: 10)
;;   (2 * 5 + 1) / 10 => 1.1 (expected: 1.1)
;;   var x =  1 / 2 => 0.5 (expected: 0.5)
;;   var y = x * 2 => 1.0 (expected: 1)
;;   9 + 10 => 19.0 (expected: 19)
;;   (2 * 5 + 1) / 10 => 1.1 (expected: 1.1)
;;   var x =  1 / 2 => 0.5 (expected: 0.5)
;;   var y = x * 2 => 1.0 (expected: 1)
;;   (x + 2) * (y * (5 - 100)) => -237.5 (expected: -237.5)
;;   var z = 5*-3.14 => -15.700000000000001 (expected: -15.7)
;;   2.6^(2 + 3/2) * (2-z) => 501.62593733169757 (expected: 501.625937332)
;;   var a = 10 => 10.0 (expected: 10.0)
;;   fun a() = 2 => #f (expected: #f)
;;   a() + a => 12.0 (expected: 12.0)
;;   fun avg(a, b) = (a + b) / 2 => #f (expected: #f)
;;   var x = avg(69, 96) => 82.5 (expected: 82.5)
;;   avg(x, avg(a(), a)) + a => 54.25 (expected: 54.25)
