(defun get-neighbours (&optional for-testing-p)
  (interactive)
  (labels
      ((make-neighbour (pos dir)
                       (if for-testing-p
                           (list (line-number-at-pos) (current-column) dir pos (format "%c" (or (char-after pos) ?@)))
                         (char-after pos)))
       (line-neighbours (for-current-p)
                        (let ((ln (list)))
                          (when (looking-back "[[:graph:]]" 1) ; "\\s-" doesn't work with line break (on windows)
                            ;; looking-at will also work as expected at beginning of line (by returning nil)
                            (push (make-neighbour (- (point) 1) 'l) ln))
                          (unless for-current-p
                            (when (looking-at "[[:graph:]]")
                              (push (make-neighbour (point) 'm) ln)))
                          (when (looking-at ".[[:graph:]]")
                            ;; looking-at will also work as expected at end of line (by returning nil)
                            (push (make-neighbour (+ (point) 1) 'r) ln))
                          ln)))
    (let ((buffer-lines (count-lines (point-min) (point-max)))
          (cc (current-column))           ;start w/ 0
          (cl (line-number-at-pos))       ;start w/ 1
          (n (list)))
      (save-excursion
        (save-excursion
          (when (= 0 (forward-line -1)) ;only if no lines left to move, means: we could move up
            (move-to-column cc t)       ;use force-flag t to add spaces to reach given column
            (setf n (append n (line-neighbours nil)))))
        (setf n (append n (line-neighbours t)))
        (save-excursion
          ;; (when (= 0 (forward-line 1))  ;only if no lines left to move, means: we could move down
          (when (< cl buffer-lines)     ;forward-line return value is also 0 at end of buffer
            (forward-line 1)
            (move-to-column cc t)
            (setf n (append n (line-neighbours nil))))))
      n)))

(defun next-state (curr-char neighbours)
  (let ((dead-char ? ))
    (labels ((alive-p (curr-char)
                      (not (= curr-char dead-char)))) ;(not (= ?x ? ))
      (let ((len (length neighbours))
            (alive-p (alive-p curr-char)))
        (cond ((and alive-p (<= len 1)) ;(<= 3 1)
               dead-char)
              ((and alive-p (<= 2 len 3)) ;(<= 2 3 3)
               curr-char)
              ((and alive-p (>= 4 len)) ;(>= 4 3)
               dead-char)
              ((and (not alive-p) (= len 3))
               (nth (random 3) neighbours))
              (t dead-char))))))

(defun buffer-loop ()
  (labels ((max-column ()
                       (save-excursion
                         (loop until (eobp)
                               do (move-end-of-line nil)
                               maximizing (current-column) into max
                               do (forward-line 1)
                               finally (return max))))
           (fill-lines (max-column)
                       (save-excursion
                         (loop until (eobp)
                               do (move-to-column max-column t)
                               do (forward-line 1))))
           (collect-commands ()
                             (save-excursion
                               (loop until (eobp)
                                     if (eolp)
                                     collect '(forward-char)
                                     else
                                     collect (let* ((n (get-neighbours))
                                                    (c (char-after (point)))
                                                    (s (next-state c n)))
                                               (if (= c s)
                                                   '(forward-char)
                                                 `(progn (delete-char 1) (insert ,s))))
                                     do (forward-char)))))
    (goto-char (point-min))
    (fill-lines (max-column))
    (loop for command in (collect-commands)
          do (eval command))))

(defun run-bonus-buffer (&optional num-steps from-buffer)
  (let* ((num-steps (or num-steps 10))
         (from-buffer (or from-buffer (current-buffer)))
         (bonus-buffer-name (format "*bonus-%s*" (file-name-nondirectory (buffer-file-name from-buffer))))
         (bonus-buffer (get-buffer-create bonus-buffer-name)))
    (switch-to-buffer bonus-buffer nil t)
    (erase-buffer)
    (insert-buffer-substring from-buffer) (goto-char (point-min))
    (redisplay)
    (sleep-for 3)
    (dotimes (_ num-steps) (buffer-loop) (redisplay)))) ; (run-bonus-buffer 10)

(defvar *sample1* " He\nll\n o")
(defvar *sample2*
  "What?\nThis is exceedingly silly.\n\nReally, we would like some ACTUAL programming challenges around here.")

(defun run-sample-buffer (which)
  (let* ((sample-buffer-name (format "*sample%d*" which))
         (sample-buffer (get-buffer-create sample-buffer-name))
         (sample-string (if (= 1 which) *sample1* *sample2*)))
    (switch-to-buffer sample-buffer nil t)
    (erase-buffer)
    (insert sample-string)
    (buffer-loop))) ; (run-sample-buffer 1) ; (run-sample-buffer 2)
