(deftype fact-status ()
  '(member no-fake found-fake inconsistent))

(defclass coin-facts ()
  ((status :initform 'no-fake :accessor fact-status :type fact-status)
   (inconsistent-idx :initform 0 :accessor inconsistent-idx :type fixnum)
   (eq-coins :initform '() :accessor eq-coins :type list)
   (fake-coins :initform '() :accessor fake-coins :type list)
   (real-coins :initform '() :accessor real-coins :type list)))

(defun status-text (coin-facts)
  (ecase (fact-status coin-facts)
    (no-fake "no fake coins detected")
    (found-fake (format nil "~a is lighter" (fake-coins coin-facts)))
    (inconsistent "data is inconsistent")))

(defun set-inconsistent (coin-facts weight-idx)
  (setf (inconsistent-idx coin-facts) weight-idx)
  (setf (fact-status coin-facts) 'inconsistent))

(defun add-fake (coin-facts coins)
  ;; We always add real and fake in pairs, so we only need to set the fake status when adding a fake.
  ;; We add to fake-coins w/o removing from equal-coins, the equal-info is still valid and worthwhile.
  (setf (fact-status coin-facts) 'found-fake)
  (pushnew coins (fake-coins coin-facts) :test #'equal))

(defun add-real (coin-facts coins)
  ;; We add to real-coins w/o removing from equal-coins, the equal-info is still valid and worthwhile.
  (pushnew coins (real-coins coin-facts) :test #'equal))

(defun add-equal (coin-facts coins)
  (pushnew coins (eq-coins coin-facts) :test #'equal))

(defun is-fake (coin-facts coins)
  (member coins (fake-coins coin-facts) :test #'equal))

(defun is-real (coin-facts coins)
  (member coins (real-coins coin-facts) :test #'equal))

(defun are-equal (coin-facts left-coins right-coins)
  (and (member left-coins (eq-coins coin-facts) :test #'equal)
       (member right-coins (eq-coins coin-facts) :test #'equal)))

(defparameter *weight-funs*
  `((left  .                            ; left means that the left side is heavier
           ,(lambda (coin-facts weight-idx left-coins right-coins)
              (cond
                ((are-equal coin-facts left-coins right-coins)
                 (set-inconsistent coin-facts weight-idx))
                ((is-fake coin-facts left-coins)
                 (set-inconsistent coin-facts weight-idx))
                ((is-real coin-facts right-coins)
                 (set-inconsistent coin-facts weight-idx))
                (t
                 (add-real coin-facts left-coins)
                 (add-fake coin-facts right-coins)))))
    (right .                            ; right means that the right side is heavier
           ,(lambda (coin-facts weight-idx left-coins right-coins)
              (cond
                ((are-equal coin-facts left-coins right-coins)
                 (set-inconsistent coin-facts weight-idx))
                ((is-fake coin-facts right-coins)
                 (set-inconsistent coin-facts weight-idx))
                ((is-real coin-facts left-coins)
                 (set-inconsistent coin-facts weight-idx))
                (t
                 (add-real coin-facts right-coins)
                 (add-fake coin-facts left-coins)))))
    (equal .
           ,(lambda (coin-facts weight-idx left-coins right-coins)
              (cond
                ((and (is-fake coin-facts left-coins) (is-real coin-facts right-coins))
                 (set-inconsistent coin-facts weight-idx))
                ((and (is-real coin-facts left-coins) (is-fake coin-facts right-coins))
                 (set-inconsistent coin-facts weight-idx))
                ((or (is-fake coin-facts left-coins) (is-fake coin-facts right-coins))
                 (add-fake coin-facts left-coins)
                 (add-fake coin-facts right-coins))
                ((or (is-real coin-facts left-coins) (is-real coin-facts right-coins))
                 (add-real coin-facts left-coins)
                 (add-real coin-facts right-coins))
                (t
                 (add-equal coin-facts left-coins)
                 (add-equal coin-facts right-coins)))))))

(defun apply-weight-fun (coin-facts weight-idx result left-coins right-coins)
  (funcall (cdr (assoc result *weight-funs*)) coin-facts weight-idx left-coins right-coins))

(defun split-coin-labels (side-symbol)
  (mapcar #'intern (mapcar #'string (sort (coerce (string side-symbol) 'list) #'char<))))

(defun exec-weigh (weights)
  (let ((facts (make-instance 'coin-facts)))
    (loop for weight-idx fixnum from 1
       for (left-coins right-coins result) in weights
       do (apply-weight-fun facts weight-idx result (split-coin-labels left-coins) (split-coin-labels right-coins)))
    (format t "~a~%" (status-text facts))))

(defun main ()
  (exec-weigh '((a b left)
                (a c equal)))
  (exec-weigh '((a c equal)))
  (exec-weigh '((a c equal)
                (a b equal)
                (c b left))))

;;; See: https://www.reddit.com/r/dailyprogrammer/comments/4utlaz/20160727_challenge_277_intermediate_fake_coins/

;;; Result for (main):
;;;   ((B)) is lighter
;;;   no fake coins detected
;;;   data is inconsistent
