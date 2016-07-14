(defpackage :challenge-242
  (:documentation "Daily Programmer: Challenge #248 [Hard] NotClick game.")
  (:use :cl))
(in-package :challenge-242)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(deftype wfloat () 'rational)

(defparameter *verbose-print-stats* nil)
(defparameter *build-cost-growth* 6/5)
(defparameter *upgrade-cost-growth* 3)
(defparameter *auto-gen-per-turn* 1)

(declaim (inline build-idx-p safe-idx safe-queue-idx next-queue-building))

(defun build-idx-p (idx)
  (< idx 100))

(defun safe-idx (idx)
  (declare (type (and unsigned-byte fixnum) idx))
  (if (build-idx-p idx)
      idx
      (- 100 idx)))

(defun safe-queue-idx (state queue-idx)
  (or (<= 0 queue-idx (1- (length (buildings state))))
      (<= 0 (- queue-idx 100) (1- (length (buildings state))))))

(defun next-queue-building (state)
  (let ((next-idx (first (build-queue state))))
    (aref (buildings state) (safe-idx next-idx))))

(defclass building ()
  ((name :initarg :name :initform (error "Must supply name")
         :reader type-name :type symbol)
   ;; Will grow by *build-cost-growth* with each new building.
   (build-cost :initarg :build-cost :initform (error "Must supply build-cost")
               :accessor build-cost :type wfloat)
   ;; Will grow by *upgrade-cost-growth* with each upgrade.
   (upgrade-cost :initarg :upgrade-cost :initform (error "Must supply upgrade-cost")
                 :accessor upgrade-cost :type wfloat)
   ;; Number of Cookies each building generates.
   (production :initarg :production :initform (error "Must supply production")
               :accessor production :type wfloat)
   ;; Number of extra cookies the building generates on first upgrade.
   (boost :initarg :boost :initform (error "Must supply boost")
          :accessor boost :type wfloat)
   ;; For statistics.
   (num-buildings :initform 0 :accessor num-buildings :type (and unsigned-byte fixnum))
   (num-upgrades :initform 0 :accessor num-upgrades :type (and unsigned-byte fixnum))
   (sum-turns :initform 0 :accessor sum-turns :type (and unsigned-byte fixnum))
   (cash-generated :initform 0 :accessor cash-generated :type wfloat)
   (cash-expenses :initform 0 :accessor cash-expenses :type wfloat)))

(defmethod print-object ((b building) out)
  (print-unreadable-object (b out :type nil :identity nil)
    (if *verbose-print-stats*
        (format out "B: ~a (P ~,2,F, BC ~,2,F, UC ~,2,F, gen ~,2,F, exp ~,2,F, r$$ ~,4,F, #B ~d, #U ~d)"
                (type-name b) (production b) (build-cost b) (upgrade-cost b)
                (cash-generated b) (cash-expenses b)
                (if (plusp (sum-turns b)) (/ (cash-generated b) (cash-expenses b) (sum-turns b)) 0)
                (num-buildings b) (num-upgrades b))
        (format out "B: ~a (P ~,2,F, BC ~,2,F, UC ~,2,F)"
                (type-name b) (production b) (build-cost b) (upgrade-cost b)))))

(defgeneric special-production (building-idx building)
  (:method ((building-idx (eql 0)) building)
    (production building))
  (:method (building-idx building)
    0))

(defun make-building-of-type (building build-cost)
  (assert (= build-cost (build-cost building)))
  (incf (num-buildings building))
  (incf (cash-expenses building) build-cost)
  ;; Apply *build-cost-growth* cost growth per purchase of the same building.
  (setf (build-cost building) (* build-cost *build-cost-growth*)))

(defun upgrade-building (building upgrade-cost)
  (assert (= upgrade-cost (upgrade-cost building)))
  (if (zerop (num-upgrades building))
      ;; Add number of extra cookies the building generates on first upgrade.
      (incf (production building) (boost building))
      ;; All subsequent upgrades double production.
      (incf (production building) (production building)))
  (incf (num-upgrades building))
  (incf (cash-expenses building) upgrade-cost)
  ;; Apply *upgrade-cost-growth* for cost increase for each upgrade of the same building.
  (setf (upgrade-cost building) (* upgrade-cost *upgrade-cost-growth*)))

(defclass state ()
  ((name :initarg :name :initform (error "Must supply name") :reader state-name :type symbol)
   (buildings :initarg :buildings :initform (error "Must supply buildings")
                   :reader buildings :type simple-array)
   (build-queue :initform () :accessor build-queue :type list)
   ;; For statistics.
   (cash-available :initform 0 :accessor cash-available :type wfloat)
   (cash-generated :initform 0 :accessor cash-generated :type wfloat)))

(defgeneric add-cash-produced (self production &key add-turns-p)
  (:method ((self state) production &key add-turns-p)
    (declare (ignore add-turns-p))
    (when (plusp production)
      (incf (cash-available self) production)
      (incf (cash-generated self) production)))
  (:method ((self building) production &key add-turns-p)
    (when (plusp production)
      (incf (cash-generated self) production)
      (when add-turns-p
        (incf (sum-turns self) (num-buildings self))))))

(defmethod print-object ((s state) out)
  (let ((sum-buildings (loop for b across (buildings s)
                          sum (num-buildings b)))
        (sum-upgrades (loop for b across (buildings s)
                         sum (num-upgrades b)))
        (next-build-cost (if (build-queue s)
                             (build-cost (next-queue-building s))
                             0)))
    (print-unreadable-object (s out :type nil :identity nil)
      (if *verbose-print-stats*
          (format out "S: ~a ($ ~,2,F [gen ~,2,F, nc ~,2,F], #B ~d, #U ~d~%  Q ~a~%~{  ~a~^~%~})"
                  (state-name s) (cash-available s) (cash-generated s) next-build-cost
                  sum-buildings sum-upgrades
                  (build-queue s)
                  (remove-if (lambda (b) (zerop (num-buildings b))) (coerce (buildings s) 'list)))
          (format out "S: ~a ($ ~,2,F [gen ~,2,F, nc ~,2,F], #B ~d, #U ~d, #Q ~d)"
                  (state-name s) (cash-available s) (cash-generated s) next-build-cost
                  sum-buildings sum-upgrades (length (build-queue s)))))))

(defun turn (turn-idx state &key print-stats-p)
  (labels ((maybe-build (building-idx)
             (let* ((building (aref (buildings state) building-idx))
                    (build-cost (build-cost building)))
               (when (<= build-cost (cash-available state))
                 (make-building-of-type building build-cost)
                 (decf (cash-available state) build-cost)
                 t)))

           (maybe-upgrade (building-idx)
             (let* ((building (aref (buildings state) building-idx))
                    (upgrade-cost (upgrade-cost building)))
               (when (<= upgrade-cost (cash-available state))
                   (upgrade-building building upgrade-cost)
                   (decf (cash-available state) upgrade-cost)
                   t))))

    ;; 1. Add resources created from buildings.
    (loop for building across (buildings state)
       do (let ((production (* (production building) (num-buildings building))))
            (add-cash-produced state production)
            (add-cash-produced building production :add-turns-p t)))
    ;; 2.a Add automatic resources from turn: These are from the 1 click per turn.
    (add-cash-produced state *auto-gen-per-turn*)
    ;; 2.b Add special production, for cursor only.
    (let* ((cursor-type (aref (buildings state) 0))
           (special-production (* (special-production 0 cursor-type) (num-buildings cursor-type))))
      (add-cash-produced state special-production)
      (add-cash-produced cursor-type special-production))
    ;; 3. If possible, buy the first building or upgrade in queue.
    (when (build-queue state)           ;don't stop on empty build queue, just continue w/ existing buildings
      (let ((next-building-idx (first (build-queue state))))
        (if (build-idx-p next-building-idx)
            (when (maybe-build next-building-idx)
              (pop (build-queue state)))
            (when (maybe-upgrade (- next-building-idx 100))
              (pop (build-queue state))))))
    (let ((*verbose-print-stats* print-stats-p))
      (format t "T ~d: ~a~%" turn-idx state))))

(defun main (&key max-turns queue)
  (let* ((dflt-building-params '((cursor 12 1/10 1/10 100)
                                      (grandma 100 8/10 3/10 1000)
                                      (farm 500 4 1 10000)
                                      (mine 1000 10 3 50000)
                                      (factory 5000 40 10 200000)
                                      (bank 100000 100 40 5000000)
                                      (temple 1000000 400 100 100000000)
                                      (city 300000000 5000 2000 1000000000)))
         (dflt-buildings
          (make-array (length dflt-building-params)
                      :element-type 'building
                      :initial-contents (loop for (name build-cost production boost upgrade-cost)
                                           in dflt-building-params
                                           collect (make-instance 'building :name name
                                                                  :build-cost build-cost :upgrade-cost upgrade-cost
                                                                  :production production :boost boost))))
         (state (make-instance 'state :name 'test
                               :buildings dflt-buildings)))
    (loop for building-idx in (reverse queue)
       when (safe-queue-idx state building-idx)
       do (push building-idx (build-queue state)))
    (format t "START: ~a~%" state)
    (loop for turn-idx from 1 upto max-turns
       do (turn turn-idx state :print-stats-p (= turn-idx max-turns)))))

;; See here: https://www.reddit.com/r/dailyprogrammer/comments/40035o/20160108_challenge_248_hard_notclick_game/

;; (main :max-turns 20 :queue '(0 0 1)) ; OK - 21.6│9.6
;; (main :max-turns 50 :queue '(0 0 0)) ; OK - 65.4│21.72
;; (main :max-turns 1000 :queue '(0 0 0 0)) ; OK - 1775.4│1710.98
;; (main :max-turns 1000 :queue '(0 0 0 0 0)) ; OK - 1962.6│1873.3
;; (main :max-turns 1000 :queue '(0 0 0 3 0)) ; OK - 5032.6│3968.18
;; (main :max-turns 300 :queue '(0 0 0 1 0 2)) ; OK - 664.6│0.184
;; (main :max-turns 300 :queue '(0 0 1 100 0 0 1 1 2)) ; OK - 905.2│376.784
;; (main :max-turns 265 :queue '(0 0 0 0 0 100 0 0 0 2)) ; OK - 799.6│1.61098
;; (main :max-turns 1000 :queue '(0 0 0 1 0 0 0 100 0 0 0 2 0 100 0 0 1 0 0 100 0 0 100 0 0 0 3 3 0 3 1 1 4 3 2 3 4 2 4 3 2 4 0 1)) ; OK - 118484│71585.4
;;   ...
;;   T 1000: #<S: TEST ($ 71585.41 [gen 118483.80, nc 0.00], #B 40, #U 4
;;     Q NIL
;;     #<B: CURSOR (P 1.60, BC 552.06, UC 8100.00, gen 36466.60, exp 6700.31, r$$ 0.0004, #B 21, #U 4)>
;;     #<B: GRANDMA (P 0.80, BC 248.83, UC 1000.00, gen 2127.20, exp 744.16, r$$ 0.0011, #B 5, #U 0)>
;;     #<B: FARM (P 4.00, BC 1036.80, UC 10000.00, gen 6660.00, exp 2684.00, r$$ 0.0015, #B 4, #U 0)>
;;     #<B: MINE (P 10.00, BC 2985.98, UC 50000.00, gen 23590.00, exp 9929.92, r$$ 0.0010, #B 6, #U 0)>
;;     #<B: FACTORY (P 40.00, BC 10368.00, UC 200000.00, gen 48640.00, exp 26840.00, r$$ 0.0015, #B 4, #U 0)>)>
