(defpackage :challenge-245 (:use :cl))
(in-package :challenge-245)

;; (declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
(declaim (optimize (debug 0) (safety 0) (speed 3) (space 2) (compilation-speed 0)))

;; === input handling

(defparameter *ip-range-tree* (interval:make-tree)
  "Storage for the interval tree as built by cl-interval.")

(defparameter *ip-range-lines*
  '("123.45.17.8 123.45.123.45 University of Vestige"
    "123.50.1.1 123.50.10.1 National Center for Pointlessness"
    "188.0.0.3 200.0.0.250 Mayo Tarkington"
    "200.0.0.251 200.0.0.255 Daubs Haywire Committee"
    "200.0.1.1 200.255.255.255 Geopolitical Encyclopedia"
    "222.222.222.222 233.233.233.233 SAP Rostov"
    "250.1.2.3 250.4.5.6 Shavian Refillable Committee"
    "123.45.100.0 123.60.32.1 United Adverbs"
    "190.0.0.1 201.1.1.1 Shavian Refillable Committee"
    "238.0.0.1 254.1.2.3 National Center for Pointlessness")
  "IP ranges as given with challenge, used for testing.")

(defun read-range-lines (line-fn &optional pathname)
  (declare (type function line-fn))
  (if pathname
      (with-open-file (input pathname :direction :input)
        (loop for line = (read-line input nil)
           while line
           do (funcall line-fn line)))
      (loop for line in *ip-range-lines*
         do (funcall line-fn line))))

(defparameter *ip-query-lines*
  '("250.1.3.4" "123.50.1.20" "189.133.73.57" "123.50.1.21" "250.1.2.4" "123.50.1.21"
    "250.1.3.100" "250.1.3.5" "188.0.0.5" "123.50.1.100" "123.50.2.34"
    "123.50.1.100" "123.51.100.52" "127.0.0.1" "123.50.1.22" "123.50.1.21"
    "188.0.0.5" "123.45.101.100" "123.45.31.52" "230.230.230.230")
  "Lookup IPs as given with challenge, used for testing.")

(declaim (type fixnum *ip-query-cnt*))
(defvar *ip-query-cnt* 0
  "We'll simply put the number of requests into a dynamic variable.")

(defun read-query-lines (line-fn &optional pathname)
  (declare (type function line-fn))
  (if pathname
      (with-open-file (input pathname :direction :input)
        (loop for line = (read-line input nil)
           while line
           do (incf *ip-query-cnt*)
           collect (funcall line-fn line)))
      (loop for line in *ip-query-lines*
         do (incf *ip-query-cnt*)
         collect (funcall line-fn line))))

;; === main

(defun main (&optional range-pathname query-pathname do-profile)
  (labels ((count-equal-strings (string-list)
             "Count occurrences per each string in STRING-LIST and return list of conses w/ string and its count."
             (mapcar (lambda (unique-string)
                       (declare (type simple-string unique-string)
                                (values (cons simple-string fixnum)))
                       (cons unique-string
                             (count-if (lambda (string)
                                         (declare (type simple-string string))
                                         (string= string unique-string)) string-list)))
                     (remove-duplicates string-list :test #'string=)))

           (find-smallest-interval (ip)
             (let ((hits (interval:find-all *ip-range-tree* ip)))
               (when hits
                 (car (sort (the list hits)
                            (lambda (i1 i2)
                              (declare (type interval:interval i1 i2))
                              (flet ((interval-size (i)
                                       (declare (type interval:interval i)
                                                (values (unsigned-byte 32)))
                                       (- (the (unsigned-byte 32) (interval:interval-end i))
                                          (the (unsigned-byte 32) (interval:interval-start i)))))
                                (declare (inline interval-size))
                                (< (interval-size i1) (interval-size i2)))))))))

           (parse-range-line (line)
             (declare (type string line)
                      (values string string string))
             (multiple-value-bind (p r)
                 (split-sequence:split-sequence #\Space line :count 2)
               (values (car p) (cadr p) (subseq line r))))

           (parse-ip-range-lines ()
             (read-range-lines
              (lambda (line)
                (declare (type string line))
                (multiple-value-bind (s-as-string e-as-string n-as-string)
                    (parse-range-line line)
                  (let ((s (cl-cidr-notation:parse-ip s-as-string))
                        (e (cl-cidr-notation:parse-ip e-as-string))
                        (n (coerce n-as-string 'simple-string)))
                    (interval:insert *ip-range-tree*
                                     (interval:make-interval :start s :end e :name n)))))
              range-pathname))

           (process-ip-query-lines ()
             (sort (count-equal-strings
                    (read-query-lines
                     (lambda (line)
                       (declare (type string line)
                                (values simple-string))
                       (let* ((ip (cl-cidr-notation:parse-ip line))
                              (smallest-interval (find-smallest-interval ip)))
                         (if smallest-interval
                             (interval:interval-name smallest-interval)
                             (coerce "<unknown>" 'simple-string))))
                     query-pathname))
                   (lambda (k1 k2)
                     (declare (type fixnum k1 k2))
                     (> k1 k2))
                   :key #'cdr)))

    (let ((*ip-query-cnt* 0)
          result-list)
      (time (parse-ip-range-lines))
      (let ((start (the fixnum (get-internal-real-time))))
        (if do-profile
            (sb-sprof:with-profiling (:report :flat :loop nil)
                                     (setf result-list (process-ip-query-lines)))
            (setf result-list (process-ip-query-lines)))
        (let* ((end (the fixnum (get-internal-real-time)))
               (delta-sec (/ (- end start) internal-time-units-per-second))
               (req-per-sec (if (zerop delta-sec)
                                0
                                (/ *ip-query-cnt* delta-sec))))
          ;; The target speed is at least 1,000-2,000 queries per second.
          (format t "Runtime: ~g sec, queries ~d, queries/sec ~g; Query Results:~%~{~a~^~%~}~%"
                  delta-sec *ip-query-cnt* req-per-sec result-list))))))

;; (main)
;; (main #p"/tmp/ips500.txt" #p"/tmp/query100.txt")  ; 100 queries: Target: 0.07 sec (for 1,500 q/sec)
;; (main #p"/tmp/ips10k.txt" #p"/tmp/query1k.txt")   ; 1,000 queries: Target: 0.67 sec (for 1,500 q/sec)
;; (main #p"/tmp/ips10k.txt" #p"/tmp/query10k.txt")  ; 10,000 queries: Target: 6.67 sec (for 1,500 q/sec)
;; (main #p"/tmp/ips1mil.txt" #p"/tmp/query10k.txt") ; 10,000 queries: Target: 6.67 sec (for 1,500 q/sec)

;; See https://www.reddit.com/r/dailyprogrammer/comments/3xdmtw/20151218_challenge_245_hard_guess_whois/

;; === Results:

;; (main #p"/tmp/ips10k.txt" #p"/tmp/query10k.txt")
;; Parsing:
;;   0.213 seconds of real time
;;   0.212024 seconds of total run time (0.204014 user, 0.008010 system)
;;   [ Run times consist of 0.100 seconds GC time, and 0.113 seconds non-GC time. ]
;;   99.53% CPU
;; Lookup:
;;   Runtime: 1.883 sec, queries 10000, queries/sec 5310.6743

;; (main #p"/tmp/ips1mil.txt" #p"/tmp/query10k.txt")
;; Parsing:
;;   18.417 seconds of real time
;;   18.424880 seconds of total run time (17.045097 user, 1.379783 system)
;;   [ Run times consist of 6.579 seconds GC time, and 11.846 seconds non-GC time. ]
;;   100.04% CPU
;; Lookup:
;;   Runtime: 491.473 sec, queries 10000, queries/sec 20.346998

;; === Prereqs:

;; cl-cidr-notation is not in quicklisp, so do:
;; git clone https://github.com/AccelerationNet/cl-cidr-notation.git $HOME/quicklisp/local-projects/cl-cidr-notation
;; (ql:quickload "cl-cidr-notation")

;; cl-interval might be in quicklisp, I needed to change it (added the name field to the interval struct), so cloned:
;; git clone https://github.com/rpav/cl-interval.git $HOME/quicklisp/local-projects/cl-interval
;; (ql:quickload "cl-interval")

;; (ql:quickload "split-sequence")
;; (require :sb-sprof)
