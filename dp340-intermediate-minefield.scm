(module dp340
        (extern
         ;; === FFI
         (include "BearLibTerminal.h")
         (type color-t uint "uint32_t")
         (type const-string string "const char *")
         (macro %blt-open::int
           () "terminal_open")
         (macro blt-refresh::void
           () "terminal_refresh")
         (macro blt-close::void
           () "terminal_close")
         (macro blt-color::void
           (::color-t) "terminal_color")
         (macro blt-bkcolor::void
           (::color-t) "terminal_bkcolor")
         (macro blt-print::void
           (::int ::int ::const-string) "terminal_print")
         (macro blt-put::void
           (::int ::int ::int) "terminal_put")
         (macro %blt-has-input::int
           () "terminal_has_input")
         (macro blt-read::int
           () "terminal_read"))
        (main main))

(define (blt-open) ::boolean
  (> (%blt-open) 0))

(define (blt-has-input) ::boolean
  (> (%blt-has-input) 0))

;; === minefield and robot handling

(define +challenge-field+ ::bstring
  "+++++++++++++
+00000*000000
+0000000*000+
+00000000000+
+*0000000*00+
+00000000000+
M00000000000+
+++++++++++++")

(define (main argv)
  (receive (field::vector rows::int cols::int start-pos::pair)
      (read-minefield +challenge-field+)
    (blt-open)
    (trm-print-legend)
    (trm-print-minefield field rows cols start-pos)
    (blt-refresh)
    (guided-walk field rows cols start-pos start-pos)
    (sleep 2000000)
    (blt-close)))

(define (read-minefield input::bstring) ::vector
  (let* ((p (open-input-string input))
         (lines::pair (read-lines p))
         (rows::int (length lines))
         (cols::int (string-length (car lines)))
         (field::vector (make-vector (* rows cols)))
         (start-pos::pair-nil '()))
    (do ((inv-row::int 0 (+ inv-row 1))
         (lines::pair-nil lines (cdr lines)))
        ((= inv-row rows))
      (do ((col::int 0 (+ col 1)))
          ((= col cols))
        (let* ((row::int (- rows (+ inv-row 1)))
               (cell-char::bchar (string-ref (car lines) col)))
          (field-set! field rows cols row col cell-char)
          (when (eq? cell-char +robo+)
            (set! start-pos `(,row . ,col))))))
    [assert (start-pos) (not (null? start-pos))]
    (values field rows cols start-pos)))

(define (guided-walk field::vector rows::int cols::int pos::pair start-pos::pair)

  (define (trm-print-move step::int move-dir::bchar color::uint) ::void
    (let* ((total-len::int 20)
           (step-len::int (modulo step total-len)))
      (blt-color color)
      (blt-print (+ legend-x-offset step-len) (+ y-offset 6)
                 (format "~c~a" move-dir (make-string (- total-len step-len) #\ )))))

  (define (trm-key) ::int
    (if (blt-has-input) (blt-read) 0))

  (define (handle-key step::int pos::pair dir::pair) ::pair-nil
    (receive (result::symbol move-dir::bchar next-pos::pair)
        (move field rows cols pos start-pos dir)
      (trm-print-move step move-dir +robo-color+)
      (case result
        ((forbidden wall)
         (trm-blink-cell field rows cols pos +warn-color+ 750000)
         pos) ;user got his warning and can now continue
        ((mine)
         (trm-blink-cell field rows cols next-pos +warn-color+ 500000 3)
         (auto-walk step pos)
         '()) ;user screwed it, robot now takes over, showing the path
        ((exit) '())
        ((ok) next-pos)
        (else (error "handle-key" "Unexpected result" result)))))

  (define (auto-walk start-step::int auto-start-pos::pair) ::void
    (receive (found?::bbool posns::pair-nil moves::pair-nil)
        (find-path field rows cols (list auto-start-pos))
      (if found?
          (do ((step::int start-step (+ step 1))
               (posns::pair-nil (reverse posns) (cdr posns))
               (moves::pair-nil (reverse moves) (cdr moves)))
              ((or (null? posns) (null? moves)))
            (trm-print-minefield field rows cols (car posns) start-pos)
            (trm-print-move step (car moves) +mine-color+)
            (blt-refresh)
            (sleep 300000))
          (error "auto-walk" "No path found" posns))))

  (let loop ((step::int 0)
             (pos::pair pos)
             (key::int (trm-key)))
    (let ((moved-pos::pair-nil (cond
                                ((= key bltk-close)
                                 '())
                                ((= key bltk-space)
                                 (auto-walk step pos)
                                 '())
                                ((= key bltk-up)
                                 (handle-key step pos '(1 . 0)))
                                ((= key bltk-right)
                                 (handle-key step pos '(0 . 1)))
                                ((= key bltk-down)
                                 (handle-key step pos '(-1 . 0)))
                                ((= key bltk-left)
                                 (handle-key step pos '(0 . -1)))
                                (else
                                 pos))))
      (unless (null? moved-pos)
        (trm-print-minefield field rows cols moved-pos start-pos)
        (blt-refresh)
        (loop (if (= key 0) step (+ step 1)) moved-pos (trm-key))))))

(define (find-path field::vector rows::int cols::int posns-so-far::pair
                   #!optional (moves-so-far::pair-nil '()))
  (let ((start-pos::pair (car (reverse posns-so-far)))
        (pos::pair (car posns-so-far))
        (last-pos::pair-nil (if (null? (cdr posns-so-far)) '() (cadr posns-so-far)))
        (dirs::pair '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))))
    (let loop ((dirs::pair dirs))
      (receive (found-exit?::bbool posns::pair moves::pair-nil)
          (receive (result::symbol move-dir::bchar next-pos::pair)
              (move field rows cols pos start-pos (car dirs))
            (if (equal? next-pos last-pos)
                (values #f posns-so-far moves-so-far)
                (case result
                  ((forbidden wall)
                   (values #f posns-so-far moves-so-far))
                  ((mine)
                   (values #f posns-so-far moves-so-far))
                  ((exit)
                   (values #t
                           (cons next-pos posns-so-far) (cons move-dir moves-so-far)))
                  ((ok)
                   (find-path field rows cols
                              (cons next-pos posns-so-far) (cons move-dir moves-so-far)))
                  (else
                   (error "find-path" "Unexpected result" result)))))
        (if found-exit?
            (values #t posns moves)
            (if (null? (cdr dirs))
                (values #f posns moves)
                (loop (cdr dirs))))))))

(define-macro (match-moves dir-sym . move-defs)
  (define (make-move-match def)
    (list (cons (car def) (cadr def))
          `(values (cons (+ (car pos) ,(car def)) (+ (cdr pos) ,(cadr def)))
                   ,(caddr def))))
  `(match-case ,dir-sym
     ,@(append (map make-move-match move-defs)
               `((else (error "match-moves" "Unexpected direction" ,dir-sym))))))

(define (move field::vector rows::int cols::int pos::pair start-pos::pair dir::pair)
    (receive (next-pos move-dir)
        (match-moves dir
                     ( 1 0  #\N)
                     ( 0 1  #\E)
                     (-1 0  #\S)
                     ( 0 -1 #\O))
      (cond
       ((or (< (car next-pos) 0) (>= (car next-pos) rows)
            (< (cdr next-pos) 0) (>= (cdr next-pos) cols))
        (if (equal? pos start-pos)
            (values 'forbidden move-dir next-pos)
            (values 'exit move-dir next-pos)))
       ((eq? (field-ref field rows cols (car next-pos) (cdr next-pos)) +mine+)
        (values 'mine move-dir next-pos))
       ((eq? (field-ref field rows cols (car next-pos) (cdr next-pos)) +wall+)
        (values 'wall move-dir next-pos))
       (else
        (values 'ok move-dir next-pos)))))

(define (field-ref field::vector rows::int cols::int row::int col::int) ::bchar
  [assert (row rows) (and (>= row 0) (< row rows))]
  [assert (col rows) (and (>= col 0) (< col cols))]
  [assert (field rows cols) (= (vector-length field) (* rows cols))]
  (vector-ref field (+ (* row cols) col)))

(define (field-set! field::vector rows::int cols::int row::int col::int val::bchar)
  [assert (row rows) (and (>= row 0) (< row rows))]
  [assert (col rows) (and (>= col 0) (< col cols))]
  [assert (field rows cols) (= (vector-length field) (* rows cols))]
  (vector-set! field (+ (* row cols) col) val))

;; UI

(define x-offset ::int 5)
(define y-offset ::int 5)
(define legend-x-offset ::int 35)

(define +start+ ::bchar #\>)
(define +lane+  ::bchar #\0)
(define +mine+  ::bchar #\*)
(define +wall+  ::bchar #\+)
(define +robo+  ::bchar #\M)

(define +fg-color+    ::uint #xFFFFFFFF) ;(0xAARRGGBB)
(define +mine-color+  ::uint #xFFFF0000)
(define +wall-color+  ::uint #xFF777777)
(define +warn-color+  ::uint #xFFFFFF00)
(define +robo-color+  ::uint #xFF0000BB)

(define bltk-close ::int #xE0)
(define bltk-space ::int #x2C)
(define bltk-left  ::int #x50)
(define bltk-right ::int #x4F)
(define bltk-up    ::int #x52)
(define bltk-down  ::int #x51)

(define (trm-blink-cell field::vector rows::int cols::int pos::pair bgcolor::int delay-ms::int
                        #!optional (n::int 1)) ::void
  (let loop ((n::int n))
    (let ((cell-char::bchar (field-ref field rows cols (car pos) (cdr pos))))
      (blt-bkcolor bgcolor)
      (trm-print-cell rows cols pos cell-char)
      (blt-refresh)
      (sleep delay-ms)
      (blt-bkcolor #xFF000000)
      (trm-print-cell rows cols pos cell-char)
      (blt-refresh))
    (when (> n 1) (loop (- n 1)))))

(define (trm-print-cell rows::int cols::int pos::pair cell-char::bchar) ::void

  (define (map-color cell-char::bchar) ::uint
    (cond ; for whatever reason `case' doesn't work here
     ((char=? cell-char +mine+)  +mine-color+)
     ((char=? cell-char +robo+)  +robo-color+)
     ((char=? cell-char +start+) +wall-color+)
     ((char=? cell-char +wall+)  +wall-color+)
     (else                       +fg-color+)))

  (blt-color (map-color cell-char))
  (blt-put (+ x-offset (cdr pos)) (+ y-offset (- rows (+ (car pos) 1)))
           (char->integer cell-char)))

(define (trm-print-legend)
  (blt-print legend-x-offset (+ y-offset 0) "+: wall; *: mine; M: robot; 0: save")
  (blt-print legend-x-offset (+ y-offset 2) "Cursor keys: move, avoiding mines")
  (blt-print legend-x-offset (+ y-offset 3) "Space: robot finds path")
  (blt-print legend-x-offset (+ y-offset 4) "Alt-F4: exit"))

(define (trm-print-minefield field::vector rows::int cols::int pos::pair
                             #!optional (start-pos::pair-nil '()))

  (do ((row::int 0 (+ row 1)))
      ((= row rows))
    (do ((col::int 0 (+ col 1)))
        ((= col cols))
      (trm-print-cell rows cols (cons row col) (field-ref field rows cols row col))))
  (unless (null? start-pos)
    (trm-print-cell rows cols (cons (car start-pos) (cdr start-pos)) +start+))
  (trm-print-cell rows cols (cons (car pos) (cdr pos)) +robo+))

;; See: https://www.reddit.com/r/dailyprogrammer/comments/7d4yoe/20171114_challenge_340_intermediate_walk_in_a/
;; Run with: bigloo -copt "-I/path/to/BearLibTerminal_0.15.4/Include/C" -ldopt "-L/path/to/BearLibTerminal_0.15.4/Linux64/ -lBearLibTerminal" -o dp340-intermediate-minefield dp340-intermediate-minefield.scm
;; Screencast is here: https://i.imgur.com/AgZpCl8.gifv
