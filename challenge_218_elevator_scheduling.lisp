(defpackage :challenge-218 (:use :cl))
(in-package :challenge-218)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(flet ((elements-are-of-type (seq type)
         (every (lambda (x) (typep x type)) seq)))

  (deftype list-of-type (type)
    "LIST-OF-TYPE allows type definitions for lists of the same TYPE, so that e.g.
(typep '(1 2 3 4) '(list-of-type fixnum)) is true."
    (let ((predicate (gensym)))
      (setf (symbol-function predicate)
            (lambda (seq) (elements-are-of-type seq type)) )
      `(and list (satisfies ,predicate)))))

(deftype non-negative-fixnum ()
  `(integer 0 , most-positive-fixnum))

(deftype positive-fixnum ()
  `(integer 1 ,most-positive-fixnum))

(deftype positive-single-float ()
  `(single-float ,least-positive-single-float ,most-positive-single-float))

(defparameter *use-challenge-input* t) ; set to nil to use debug input

(defparameter *input*
  '(
    2
    C1 12 .1 1
    C2 12 .2 1
    359
    R3 0 1 9
    R4 1 1 11
    R0 11 1 7
    R2 11 1 9
    R15 13 1 9
    R5 26 1 4
    R16 27 1 2
    R1 28 1 2
    R13 28 1 9
    R10 32 1 3
    R14 35 1 4
    R8 36 1 10
    R17 38 1 12
    R3 49 9 9
    R18 50 1 10
    R7 51 1 3
    R10 53 3 10
    R12 54 1 6
    R0 60 7 1
    R1 62 2 1
    R9 66 1 8
    R19 66 1 6
    R15 71 9 2
    R11 72 1 8
    R16 78 2 4
    R6 82 1 12
    R8 85 10 11
    R10 89 10 12
    R3 90 9 6
    R5 94 4 7
    R2 94 9 10
    R6 95 12 1
    R3 111 6 9
    R14 114 4 5
    R13 115 9 5
    R19 117 6 2
    R12 122 6 12
    R4 123 11 7
    R9 123 8 12
    R6 124 1 5
    R0 124 1 6
    R7 127 3 3
    R11 139 8 9
    R7 141 3 4
    R17 143 12 2
    R14 143 5 5
    R16 151 4 9
    R5 155 7 12
    R1 155 1 11
    R18 159 10 10
    R15 160 2 4
    R19 162 2 3
    R2 164 10 3
    R11 164 9 9
    R3 165 9 4
    R12 167 12 1
    R10 169 12 1
    R0 174 6 9
    R11 181 9 2
    R18 182 10 12
    R9 184 12 4
    R5 185 12 11
    R4 197 7 5
    R2 198 3 3
    R3 198 4 8
    R6 199 5 5
    R8 199 11 6
    R13 201 5 5
    R14 203 5 4
    R1 205 11 12
    R16 211 9 1
    R6 212 5 11
    R7 214 4 8
    R15 216 4 6
    R19 226 3 11
    R1 230 12 12
    R7 232 8 5
    R0 234 9 12
    R3 237 8 2
    R17 238 2 6
    R2 240 3 11
    R12 240 1 3
    R15 246 6 6
    R13 247 5 10
    R5 248 11 5
    R10 249 1 6
    R18 252 12 4
    R9 253 4 8
    R1 256 12 12
    R4 257 5 12
    R16 258 1 2
    R13 258 10 5
    R6 262 11 2
    R11 263 2 7
    R9 269 8 5
    R3 271 2 6
    R14 274 4 9
    R5 282 5 12
    R11 285 7 6
    R16 287 2 8
    R14 290 9 5
    R2 297 11 4
    R18 299 4 6
    R13 300 5 5
    R8 301 6 5
    R0 303 12 3
    R19 305 11 1
    R7 310 5 8
    R2 311 4 4
    R1 315 12 8
    R16 318 8 11
    R8 320 5 8
    R1 324 8 2
    R10 325 6 9
    R17 325 6 2
    R2 330 4 11
    R19 330 1 9
    R9 332 5 5
    R5 335 12 11
    R18 338 6 9
    R11 340 6 8
    R12 342 3 9
    R9 344 5 11
    R12 346 9 12
    R13 346 5 12
    R6 351 2 2
    R0 354 3 10
    R10 358 9 9
    R4 369 12 12
    R15 370 6 8
    R3 372 6 5
    R17 374 2 9
    R14 383 5 4
    R7 389 8 1
    R18 396 9 6
    R12 396 12 7
    R8 411 8 1
    R16 419 11 3
    R2 420 11 1
    R10 420 9 11
    R6 423 2 12
    R1 423 2 8
    R7 425 1 1
    R11 426 8 4
    R13 429 12 11
    R19 430 9 7
    R5 432 11 9
    R15 435 8 3
    R0 438 10 6
    R6 444 12 9
    R17 449 9 9
    R14 452 4 4
    R9 456 11 2
    R18 460 6 7
    R5 463 9 2
    R12 464 7 2
    R4 468 12 5
    R13 468 11 6
    R2 475 1 4
    R19 478 7 4
    R12 491 2 10
    R10 496 11 7
    R0 501 6 3
    R2 501 4 2
    R7 502 1 3
    R3 502 5 7
    R14 505 4 11
    R6 507 9 2
    R1 508 8 12
    R15 510 3 1
    R16 512 3 12
    R11 515 4 10
    R18 515 7 2
    R19 517 4 3
    R15 519 1 5
    R9 521 2 2
    R2 524 2 5
    R14 525 11 2
    R18 526 2 11
    R4 530 5 5
    R6 531 2 5
    R8 536 1 5
    R12 536 10 3
    R16 536 12 7
    R15 538 5 7
    R17 538 9 5
    R13 544 6 7
    R10 546 7 11
    R11 547 10 5
    R7 548 3 1
    R4 554 5 1
    R3 558 7 11
    R10 568 11 7
    R6 570 5 5
    R12 572 3 7
    R7 573 1 4
    R19 574 3 6
    R16 576 7 3
    R0 577 3 8
    R4 586 1 9
    R11 587 5 9
    R14 587 2 4
    R2 590 5 5
    R5 599 2 2
    R10 599 7 7
    R9 601 2 4
    R1 603 12 6
    R3 606 11 1
    R18 606 11 9
    R13 610 7 11
    R10 614 7 4
    R17 615 5 4
    R16 616 3 3
    R12 617 7 10
    R7 621 4 2
    R6 622 5 4
    R19 626 6 12
    R2 628 5 11
    R15 629 7 7
    R14 630 4 4
    R11 632 9 6
    R8 632 5 3
    R0 639 8 6
    R6 649 4 10
    R10 651 4 11
    R9 653 4 6
    R14 653 4 12
    R4 655 9 10
    R0 656 6 4
    R2 660 11 5
    R13 660 11 6
    R3 663 1 6
    R18 664 9 5
    R1 667 6 7
    R5 668 2 11
    R12 668 10 9
    R16 672 3 9
    R15 675 7 4
    R17 680 4 3
    R7 681 2 10
    R9 681 6 9
    R10 686 11 10
    R14 689 12 9
    R4 690 10 3
    R1 698 7 9
    R18 698 5 8
    R0 699 4 12
    R19 705 12 7
    R2 708 5 1
    R8 712 3 8
    R13 718 6 2
    R0 721 12 7
    R14 721 9 5
    R18 722 8 7
    R15 723 4 8
    R14 730 5 11
    R4 733 3 12
    R13 738 2 4
    R6 741 10 1
    R10 741 10 1
    R15 741 8 9
    R19 743 7 2
    R13 751 4 7
    R3 752 6 1
    R14 755 11 9
    R4 758 12 2
    R11 759 6 9
    R5 762 11 9
    R15 765 9 2
    R19 770 2 6
    R9 775 9 9
    R12 777 9 12
    R17 778 3 7
    R0 780 7 3
    R0 781 3 11
    R18 785 7 1
    R8 787 8 11
    R6 788 1 11
    R7 790 10 4
    R19 791 6 7
    R13 791 7 6
    R2 792 1 1
    R9 794 9 5
    R10 800 1 10
    R15 804 2 5
    R12 807 12 1
    R11 808 9 4
    R5 809 9 5
    R14 813 9 2
    R1 819 9 11
    R19 819 7 5
    R16 822 9 4
    R0 823 11 8
    R17 828 7 2
    R11 834 4 4
    R8 834 11 11
    R3 837 1 6
    R5 839 5 4
    R4 842 2 4
    R2 844 1 11
    R18 851 1 1
    R15 854 5 8
    R0 855 8 5
    R6 857 11 11
    R12 857 1 3
    R9 858 5 11
    R8 859 11 3
    R10 863 10 5
    R7 867 4 6
    R5 869 4 6
    R0 878 5 8
    R6 879 11 12
    R7 882 6 12
    R17 883 2 10
    R13 883 6 5
    R8 885 3 11
    R13 887 5 7
    R15 888 8 6
    R3 891 6 6
    R6 898 12 10
    R17 898 10 3
    R3 899 6 5
    R5 900 6 11
    R18 901 1 9
    R15 906 6 10
    R19 907 5 12
    R13 908 7 9
    R11 914 4 5
    R16 917 4 5
    R8 924 11 11
    R14 924 2 2
    R0 926 8 9
    R9 926 11 2
    R2 935 11 7
    R1 937 11 5
    R10 940 5 8
    R18 946 9 11
    R19 946 12 4
    R3 947 5 8
    R8 947 11 4
    R13 947 9 4
    R12 948 3 4
    R4 950 4 2
    R9 951 2 9
    R0 963 9 11
    R17 973 3 3
    R16 975 5 12
    R18 977 11 12
    R9 980 9 6
    R13 980 4 9
    R5 983 11 1
    R3 983 8 11
    R7 985 12 7
    R14 985 2 8
    R10 991 8 12
    R19 991 4 6
    R17 992 3 5
    R0 993 11 6
    R1 997 5 3))

(defun parse-input (&optional (input *input*))
  (flet ((to-int (s)
           (parse-integer (princ-to-string s)))
         (to-float (s)
           (let ((*read-eval* nil)
                 (*read-default-float-format* 'single-float)) ;where single-float is already the default
             (read-from-string (princ-to-string s)))))
    (let* ((max-speed 0.0)
           (max-floor 0)
           (num-cars (pop input))
           (cars
            (loop for car-idx from 1 to num-cars
               for car-id = (pop input)
               for car-capacity = (pop input)
               for car-speed = (pop input) ;in floors per second
               for car-floor = (pop input)
               do (when (> car-speed max-speed) (setf max-speed car-speed))
               collect (make-instance 'lift-car :id car-id :capacity (to-int car-capacity)
                                      :speed (to-float car-speed) :floor (to-int car-floor))))
           (num-requests (pop input))
           (requests
            (loop for idx from 1 to num-requests
               for rider-id = (pop input)
               for timepoint = (pop input)
               for start-floor = (pop input)
               for end-floor = (pop input)
               do (when (> end-floor max-floor) (setf max-floor end-floor))
               collect (make-instance 'ride-request :id rider-id :at-time timepoint
                                      :start-floor start-floor :end-floor end-floor))))
      (declare (type float max-speed)
               (type fixnum max-floor))
      (values cars requests max-speed max-floor)))) ; (parse-input)

(defun secs-per-floor (speed)
  "Return number of floors per second, asserting that this is an integer value."
  (declare (type float speed)
           (values fixnum))
  (multiple-value-bind (quot rem)
      (floor 1 speed)
    ;; we know that this is true for all speed values in our input, and it simplifies further computation,
    ;; so just make sure that this assertion is always fulfilled.
    (assert (equal rem 0.0))
    quot))

(defclass lift-car ()
  ((car-id :initarg :id :initform (error "Must supply id") :reader car-id :type symbol)
   (capacity :initarg :capacity :initform (error "Must supply capacity") :reader car-capacity :type positive-fixnum)
   (speed :initarg :speed :initform (error "Must supply speed") :reader car-speed :type positive-single-float)
   (floor :initarg :floor :initform (error "Must supply floor") :accessor car-floor :type positive-fixnum)
   (riders :initform (list) :accessor car-riders :type (list-of-type symbol))))

(defmethod print-object ((self lift-car) out)
  (print-unreadable-object (self out :type nil :identity nil)
    (format out "C: ~a (~d/~d): s=~f, f=~d"
            (car-id self) (length (car-riders self)) (car-capacity self) (car-speed self) (car-floor self))))

(defun rider-enter-at-floor (lift-car rider-id start-floor)
  (assert (equal (car-floor lift-car) start-floor))
  (assert (not (member rider-id (car-riders lift-car))))
  (push rider-id (car-riders lift-car)))

(defun rider-leave-at-floor (lift-car rider-id end-floor)
  (assert (member rider-id (car-riders lift-car)))
  (setf (car-riders lift-car) (remove rider-id (car-riders lift-car) :test #'string=))
  (assert (not (member rider-id (car-riders lift-car))))
  (setf (car-floor lift-car) end-floor))

(defun has-capacity (lift-car &optional (how-many 1))
  (<= (+ (length (car-riders lift-car)) how-many) (car-capacity lift-car)))

(defclass ride-request ()
  ((rider-id :initarg :id :initform (error "Must supply id") :reader rider-id :type symbol)
   (at-time :initarg :at-time :initform (error "Must supply at-time") :reader at-time :type non-negative-fixnum)
   (start-floor :initarg :start-floor :initform (error "Must supply start-floor") :reader start-floor
                :type positive-fixnum)
   (end-floor :initarg :end-floor :initform (error "Must supply end-floor") :reader end-floor :type positive-fixnum)
   (done-p :initform nil :accessor done-p :type boolean)))

(defmethod initialize-instance :after ((self ride-request) &key)
  (when (= (start-floor self) (end-floor self))
    (setf (done-p self) t)))

(defmethod print-object ((self ride-request) out)
  (print-unreadable-object (self out :type nil :identity nil)
    (format out "RR ~:[A~;d~]: ~d: t=~d, f=~d..~d"
            (done-p self)
            (rider-id self)
            (at-time self) (start-floor self) (end-floor self))))

(defclass %move ()
  ((car-id :initarg :car-id :initform (error "Must supply car-id") :reader car-id :type symbol)
   (start-time :initarg :start-time :initform (error "Must supply start-time") :reader start-time
               :type non-negative-fixnum)
   (start-floor :initarg :start-floor :initform (error "Must supply start-floor") :reader start-floor
                :type positive-fixnum)
   (end-time :initarg :end-time :reader end-time :writer %end-time :type non-negative-fixnum)
   (end-floor :initarg :end-floor :initform (error "Must supply end-floor") :accessor end-floor :type positive-fixnum)
   (done-p :initform nil :accessor done-p :type boolean)))

(defmethod initialize-instance :before ((self %move) &key start-floor end-floor start-time end-time)
  (declare (type fixnum start-floor end-floor start-time end-time))
  (when (= end-floor start-floor)       ;caller has to prevent that we create a move or ride for this case
    (error "end-floor ~d must be unequal to start-floor ~d" end-floor start-floor))
  (unless (> end-time start-time)       ;then we also know that this assertion must hold
    (error "end-time ~d must be greater than start-time ~d" end-time start-time)))

(defun (setf end-time) (end-time move)
  (let ((start-time (start-time move)))
    (if (> end-time start-time)
        (setf (slot-value move '%end-time) end-time)
        (error "end-time ~f must be unequal to start-time ~f" end-time start-time))))

(defclass move (%move)
  ((for-request :initarg :for-request :initform (error "Must supply for-request") :reader for-request
                :type ride-request)))

(defmethod print-object ((self move) out)
  (print-unreadable-object (self out :type nil :identity nil)
    (format out "M ~:[A~;d~]: ~a: t=~d..~d, f=~d..~d"
            (done-p self)
            (car-id self)
            (start-time self) (end-time self) (start-floor self) (end-floor self))))

(defun make-move (lift-car ride-request &key start-time)
  (let* ((start-floor (car-floor lift-car))
         (end-floor (start-floor ride-request))
         (move-duration (ceiling (abs (- end-floor start-floor)) (car-speed lift-car)))
         (end-time (+ start-time move-duration))
         (move (make-instance 'move
                              :for-request ride-request
                              :car-id (car-id lift-car)
                              :start-time start-time
                              :start-floor start-floor
                              :end-floor end-floor
                              :end-time end-time)))
    move))

(defclass ride (%move)
  ((rider-id :initarg :rider-id :initform (error "Must supply rider-id") :reader rider-id :type symbol)
   (req-time :initarg :req-time :initform (error "Must supply req-time") :reader req-time :type non-negative-fixnum)))

(defmethod print-object ((self ride) out)
  (print-unreadable-object (self out :type nil :identity nil)
    (format out "R ~:[A~;d~]: ~a@~a: t=~d..~d (~d), f=~d..~d"
            (done-p self)
            (rider-id self) (car-id self)
            (start-time self) (end-time self) (req-time self) (start-floor self) (end-floor self))))

(defun make-ride (lift-car ride-request &key start-time start-floor end-floor)
  (when start-time
    (assert (>= start-time (at-time ride-request))))
  (let* ((start-time (or start-time (at-time ride-request)))
         (start-floor (or start-floor (start-floor ride-request)))
         (end-floor (or end-floor (end-floor ride-request)))
         (ride-duration (ceiling (abs (- end-floor start-floor)) (car-speed lift-car)))
         (end-time (+ start-time ride-duration))
         (ride (make-instance 'ride
                              :car-id (car-id lift-car)
                              :rider-id (rider-id ride-request)
                              :start-time start-time
                              :start-floor start-floor
                              :end-floor end-floor
                              :end-time end-time
                              :req-time (at-time ride-request))))
    ride))

;; === strategy

(defclass strategy ()
  ((max-speed :initarg :max-speed :initform (error "Must supply max-speed") :reader max-speed
              :type positive-single-float)
   (max-floor :initarg :max-floor :initform (error "Must supply max-floor") :reader max-floor
              :type positive-fixnum)))

(defclass simple-strategy (strategy)
  ())

(defclass closest-car-strategy (strategy)
  ())

(defclass farmost-car-strategy (strategy)
  ())

(defclass closest-car-by-speed-strategy (strategy)
  ((quick-floors :initarg :quick-floors :initform (error "Must supply max-floor") :reader quick-floors
                 :type non-negative-fixnum)))

(declaim (inline free-cars))
(defun free-cars (cars)
  (declare (type list cars)
           (values list))
  (remove-if-not #'has-capacity cars))

(defgeneric find-best-due-requests (strategy requests time-point cars rides)
  (:method ((strategy strategy) requests time-point cars rides)
    (let ((riders-already-served (list)))
      ;; We cannot serve all requests in time, so new ride-requests might be pending for riders still in rides.
      ;; Note: since requests come in with ascending time, we'll already serve oldest requests first.
      (loop for car in cars
         do (loop for rider-id in (car-riders car)
               do (pushnew rider-id riders-already-served)))
      (remove-if (lambda (request)
                   (if (or (< time-point (at-time request))                   ;ride not yet due
                           (member (rider-id request) riders-already-served)) ;rider already served
                       t                                                      ;don't make ride for that request
                       (progn                                                 ;else: request can be served
                         (pushnew (rider-id request) riders-already-served)   ;so store our rider as being served
                         nil)))
                 (remove-if #'done-p requests)))))

(defgeneric find-best-car (strategy cars start-floor end-floor)
  (:method ((strategy simple-strategy) cars start-floor end-floor)
    (declare (values (or null lift-car))
             (ignore start-floor end-floor))
    (first (free-cars cars)))

  (:method ((strategy closest-car-strategy) cars start-floor end-floor)
    (declare (type positive-fixnum start-floor end-floor)
             (values (or null lift-car))
             (ignore end-floor)
             ;; stop SBCL finding (and complaining) about the case where we have just 1 car and hence sorting is useless
             (sb-ext:muffle-conditions sb-ext:code-deletion-note))
    (first (sort (free-cars cars)
                 (lambda (f1 f2)
                   (declare (type positive-fixnum f1 f2))
                   (< (the positive-fixnum (abs (- f1 start-floor)))
                      (the positive-fixnum (abs (- f2 start-floor)))))
                 :key #'car-floor)))

  (:method ((strategy farmost-car-strategy) cars start-floor end-floor)
    (declare (type positive-fixnum start-floor end-floor)
             (values (or null lift-car))
             (ignore end-floor)
             (sb-ext:muffle-conditions sb-ext:code-deletion-note))
    (first (sort (free-cars cars)
                 (lambda (f1 f2)
                   (declare (type positive-fixnum f1 f2))
                   (> (the positive-fixnum (abs (- f1 start-floor)))
                      (the positive-fixnum (abs (- f2 start-floor)))))
                 :key #'car-floor)))

  (:method ((strategy closest-car-by-speed-strategy) cars start-floor end-floor)
    (declare (type positive-fixnum start-floor end-floor)
             (values (or null lift-car))
             (sb-ext:muffle-conditions sb-ext:code-deletion-note))
    (let* ((diff-floors (abs (- end-floor start-floor)))               ;car has to travel that many floors
           (find-quick-car-p (> diff-floors (quick-floors strategy)))) ;if request has to many floors, find quick car
      (first (sort (free-cars cars)
                   #'<
                   :key (lambda (car)
                          ;; the 1+ is here to avoid rel-floors = 0 if start floor is current car's floor
                          (let* ((rel-floors (1+ (/ (abs (- (car-floor car) start-floor))
                                                    (max-floor strategy))))
                                 (rel-car-speed (/ (car-speed car)
                                                   (max-speed strategy)))
                                 (comparison-key (if find-quick-car-p
                                                     (/ rel-floors rel-car-speed)    ;prefer quicker cars
                                                     (* rel-floors rel-car-speed)))) ;prefer slower cars
                            (the single-float (coerce comparison-key 'single-float)))))))))

;; === main

(defun main (strategy-class-symbol &rest args)
  (flet ((finished-moves (moves time-point)
           (remove-if (lambda (move) (< time-point (end-time move)))
                      (remove-if #'done-p moves)))
         (finished-rides (rides time-point)
           (remove-if (lambda (ride) (< time-point (end-time ride)))
                      (remove-if #'done-p rides)))
         (find-car-by-id (cars car-id)
           (find car-id cars :key #'car-id :test #'eq)))

    (declare (inline finished-moves finished-rides find-car-by-id))
    (multiple-value-bind (challenge-cars challenge-requests challenge-max-speed challenge-max-floor)
        (parse-input)
      (let* ((cars (if *use-challenge-input*
                       challenge-cars
                       (list (make-instance 'lift-car :id 'C1 :capacity 12 :speed 0.1 :floor 1)
                             (make-instance 'lift-car :id 'C2 :capacity 12 :speed 0.2 :floor 1))))
             (requests (if *use-challenge-input*
                           challenge-requests
                           (list (make-instance 'ride-request :id 'R3 :at-time 0 :start-floor 1 :end-floor 9)
                                 (make-instance 'ride-request :id 'R4 :at-time 1 :start-floor 1 :end-floor 11))))
             (max-speed (if *use-challenge-input*
                            challenge-max-speed
                            0.2))
             (max-floor (if *use-challenge-input*
                            challenge-max-floor
                            12))
             (moves (list))
             (rides (list))
             (max-capacity 0)
             (max-sum-capacity 0)
             (strategy (apply #'make-instance strategy-class-symbol :max-speed max-speed :max-floor max-floor args)))

        (multiple-value-bind (time-delta handle-requests-duration)
            (loop named loop-requests
               with time-delta of-type fixnum = (secs-per-floor max-speed) ;speed is in floors per second, so ...
               for time-point of-type fixnum from 0 by time-delta          ;... time-point is time in seconds
               do (loop for car in cars ;update max capacity used per car and over all cars
                     with sum-capacity = 0
                     for capacity = (car-capacity car)
                     do (setf sum-capacity (+ sum-capacity capacity))
                     when (> capacity max-capacity)
                     do (setf max-capacity capacity)
                     finally (setf max-sum-capacity sum-capacity))
               do (loop for move in (finished-moves moves time-point) ;process moves just finished in last time slice
                     do (let ((move-car (find-car-by-id cars (car-id move)))
                              (request (for-request move)))
                          (setf (done-p move) t)
                          ;; now also update that car arrived at end-floor of move, car is at correct floor, we'll make
                          ;; a ride from this below (find-best-due-requests ...), not already here
                          (setf (car-floor move-car) (start-floor request))))
               do (loop for ride in (finished-rides rides time-point) ;process rides just finished in last time slice
                     do (let ((ride-car (find-car-by-id cars (car-id ride))))
                          (rider-leave-at-floor ride-car (rider-id ride) (end-floor ride))
                          (setf (done-p ride) t)))
               do (loop for request in (find-best-due-requests strategy ;find best requests due in the next time slice
                                        requests time-point cars rides)
                     for best-car = (find-best-car strategy             ;try to find the best car with open capacity
                                                   cars (start-floor request) (end-floor request))
                     when best-car
                     do (if (equal (car-floor best-car) (start-floor request))
                            (progn
                              (push (make-ride best-car request :start-time time-point) rides)
                              (rider-enter-at-floor best-car (rider-id request) (start-floor request))
                              (setf (done-p request) t))
                            (push (make-move best-car request :start-time time-point) moves)))
               do (let ((open-requests (remove-if #'done-p requests)))
                    (if open-requests
                        (return-from loop-requests (values time-delta time-point)))))

          (format t "#-rr: ~d (#-or: ~d) #-m: ~d #-r: ~d -> dur: ~d (dt: ~d, mc: ~d, smc: ~d)~%"
                  (length requests) (length (remove-if #'done-p requests))
                  (length moves) (length rides)
                  handle-requests-duration time-delta max-capacity max-sum-capacity))))))

;; (main 'simple-strategy)
;; (main 'closest-car-strategy)
;; (main 'farmost-car-strategy)
(main 'closest-car-by-speed-strategy :quick-floors 3)

;; https://www.reddit.com/r/dailyprogrammer/comments/39ixxi/20150612_challenge_218_hard_elevator_scheduling/

;; I just started to work on this long after it was posted, so I did not any more post my solution.
;; It would still require some work to find the best result, but sadly not much happened on that challenge,
;; even if I found that one of the most interesting challenges.
;; E.g. adding some more parameters to the strategies and having an GA search for the optimal solution could
;; be a perfect challenge for next vacation - if there would not be so many interesting things to do...
