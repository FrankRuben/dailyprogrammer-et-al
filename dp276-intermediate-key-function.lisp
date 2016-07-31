(ql:quickload "split-sequence")

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defun key (fn values &key keys with-key-p as-list-p)
  (let ((groups '())
        (sequence '()))
    (loop for v in values
       for k in (or keys values)
       for group = (assoc k groups)
       do (pushnew k sequence)
       if group do (push v (cdr group))
       else do (push (cons k (list v)) groups))
    (loop for k in (reverse sequence)
       for group = (cdr (assoc k groups))
       for res = (if as-list-p
                     (funcall fn group)
                     (apply fn group))
       collect (if with-key-p
                   (cons k res)
                   (apply fn group)))))

(defun main ()
  (labels ((split-integers (line)
             (mapcar #'parse-integer (split-sequence:split-sequence #\Space line)))
           (split-symbols (line)
             (mapcar #'intern (split-sequence:split-sequence #\Space line))))
    (assert (equal (key #'+
                        (split-integers "3 4 5 6")
                        :keys (split-integers "2 0 1 2"))
                   '(9 4 5)))
    (princ (key #'length
                (split-integers "5 3 5 2 2 9 7 0 7 5 9 2 9 1 9 9 6 6 8 5 1 1 4 8 5 0 3 5 8 2 3 8 3 4 6 4 9 3 4 3 4 5 9 9 9 7 7 1 9 3 4 6 6 8 8 0 4 0 6 3 2 6 3 2 3 5 7 4 2 6 7 3 9 5 7 8 9 5 6 5 6 8 3 1 8 4 6 5 6 4 8 9 5 7 8 4 4 9 2 6 10")
                :with-key-p t :as-list-p t))
    (princ (key #'+
                (split-integers "14 21 82 85 54 96 9 61 43 49 16 34 73 59 36 24 45 89 77 68")
                :keys (split-symbols "a b c d a b c d a b c d a b c d a b c d")
                :with-key-p t))
    (princ (key #'first
                (split-integers "14 21 82 85 54 96 9 61 43 49 16 34 73 59 36 24 45 89 77 68")
                :keys (split-symbols "a b c d a b c d a b c d a b c d a b c d")
                :with-key-p t :as-list-p t))))

(main)

;; See: https://www.reddit.com/r/dailyprogrammer/comments/4tqy5c/20160720_challenge_276_intermediate_key_function/
;;
;; Output for main:
;;    ((5 . 13) (3 . 12) (2 . 8) (9 . 14) (7 . 8) (0 . 4) (1 . 5) (6 . 13) (8 . 11) (4 . 12) (10 . 1))
;;    ((a . 229) (b . 314) (c . 220) (d . 272))
;;    ((a . 45) (b . 89) (c . 77) (d . 68))
