(module dp258
        (main main))

(define (main argv)

  (define (make-irc-client-socket server-hostname::string server-port::int buf-sz::int)
    (with-handler (lambda (e)
                    (printf "Error opening socket on ~a:~a~%~a" server-hostname server-port e)
                    #f)
                  (make-client-socket server-hostname server-port :inbuf buf-sz :outbuf buf-sz)))

  (define (parse-server server::string)
    (let* ((parts::pair (string-split server ":"))
           (server-hostname::string (car parts))
           (server-port::int (string->integer (cadr parts))))
      (values server-hostname server-port)))

  (let ((server (list-ref argv 1))
        (nickname (list-ref argv 2))
        (username (list-ref argv 3))
        (realname (list-ref argv 4)))

    (receive (server-hostname::string server-port::int)
             (parse-server server)

             (printf "Trying to connect ~a:~a~%" server-hostname server-port)
             (let ((socket (make-irc-client-socket server-hostname server-port 2048)))
               (if socket
                   (let ((in-port (socket-input socket))
                         (out-port (socket-output socket)))

                     (fprintf out-port "NICK ~a~c~c" nickname #\Newline #\Return)
                     (fprintf out-port "USER ~a 0 * :~a~c~c" username realname #\Newline #\Return)
                     (flush-output-port out-port)
                     (printf "Connected~%")

                     (let ((state::symbol 'wait-for-ping))
                       (let loop ((line (read-line in-port)))
                         (if (not (eof-object? line))
                             (let ((parts::pair (string-split line)))
                               (cond
                                ((and (eq? state 'wait-for-ping)
                                      (string=? (car parts) "PING"))
                                 (fprintf out-port "PONG ~l~c~c" (cdr parts) #\Newline #\Return)
                                 (flush-output-port out-port)
                                 (set! state 'pong-sent))
                                ((eq? state 'pong-sent)
                                 (fprintf out-port "QUIT~c~c" #\Newline #\Return)
                                 (flush-output-port out-port)
                                 (set! state 'quit-sent)))
                               (loop (read-line in-port))))))

                     (printf "Closing~%")
                     (socket-close socket)))))))

;;; See: https://www.reddit.com/r/dailyprogrammer/comments/4ad23z/20160314_challenge_258_easy_irc_making_a/

;;; Using Bigloo Scheme
;;;   Connection parameters are read from command line, not from file. 
;;;   So start e.g. with bigloo dp258-irc.scm && ./a.out chat.freenode.net:6667 xyznick_ xyzuser_ xyzreal_.
