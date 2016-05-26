(defun eccentricity-from-bfs (nodes start-idx nb-nodes)
  ;; Eccentricity of a vertex is its shortest path distance from the farthest other node in the graph.
  (declare (type (array bit 2) nodes)
           (type fixnum start-idx nb-nodes)
           (values fixnum))
  (assert (= nb-nodes (array-dimension nodes 1)))

  (labels ((make-queue ()
             '())

           (queue-empty (q)
             (null q))

           (queue-push (q item)
             (append q (list item)))

           (queue-pop (q)
             (assert (not (queue-empty q)))
             (values (cdr q) (car q))))

    (declare (inline make-queue queue-empty queue-push queue-pop))

    (let ((visited (make-array nb-nodes :element-type 'bit :initial-element 0))
          (distance (make-array nb-nodes :element-type 'fixnum :initial-element 0))
          (path-queue (make-queue)))

      (labels ((share-edge (from-idx to-idx)
                 (= (aref nodes from-idx to-idx) 1))

               (is-unvisited (idx)
                 (= (aref visited idx) 0))

               (push-unvisited (idx &optional (dist 0))
                 (assert (= (aref visited idx) 0))
                 (setf path-queue (queue-push path-queue idx))
                 (setf (aref visited idx) 1)
                 (when (plusp dist)
                   (setf (aref distance idx) dist)))

               (pop-visited ()
                 (multiple-value-bind (new-q visited-idx)
                     (queue-pop path-queue)
                   (assert (= (aref visited visited-idx) 1))
                   (setf path-queue new-q)
                   (cons visited-idx
                         (aref distance visited-idx)))))

        (declare (inline share-edge is-unvisited))

        (push-unvisited start-idx)
        (loop until (queue-empty path-queue)
           for (next-idx . curr-dist) = (pop-visited)
           do (loop for other-idx from 0 below nb-nodes
                 when (and (/= next-idx other-idx)
                           (share-edge next-idx other-idx)
                           (is-unvisited other-idx))
                 do (push-unvisited other-idx (1+ curr-dist))))
        (apply #'max (coerce distance 'list))))))

(defun radius-diameter (nodes &aux (nb-nodes (array-dimension nodes 0)))
  ;; The diameter of a graph is the maximum eccentricity of any vertex in the graph.
  ;; The radius of a graph is the minimum eccentricity of any vertex.
  (declare (type (array bit 2) nodes)
           (type fixnum nb-nodes)
           (values fixnum fixnum))
  (loop for node-idx fixnum from 0 below nb-nodes
     for e fixnum = (eccentricity-from-bfs nodes node-idx nb-nodes)
     maximizing e into diameter
     when (plusp e)
     minimizing e into radius
     finally (return (values radius diameter))))

(defun center-nodes (nodes radius &aux (nb-nodes (array-dimension nodes 0)))
  ;; The center of G is the set of vertices of eccentricity equal to the radius of the graph.
  (declare (type (array bit 2) nodes)
           (type fixnum radius nb-nodes))
  (loop for node-idx fixnum from 0 below nb-nodes
     for e fixnum = (eccentricity-from-bfs nodes node-idx nb-nodes)
     when (= e radius)
     collect (1+ node-idx)))

(defun process (lines &key directed-p)
  (with-input-from-string (input lines)
    (let* ((n (read input))
           (v (make-array `(,n ,n) :element-type 'bit)))

      (loop for n1 = (read input nil)
         while n1
         for n2 = (read input nil)
         do (setf (sbit v (1- n1) (1- n2)) 1)
         unless directed-p
         do (setf (sbit v (1- n2) (1- n1)) 1))

      (multiple-value-bind (r d)
          (radius-diameter v)
        (format t "Number of nodes: ~d [~a], radius: ~d, diameter: ~d~%"
                n (if directed-p 'directed 'undirected) r d)
        (format t "Center nodes: ~a~%"
                (center-nodes v r))))))

;; See https://www.reddit.com/r/dailyprogrammer/comments/4iut1x/20160511_challenge_266_intermediate_graph_radius/
;;
;; Output for sample and challenge:
;;
;; Number of nodes: 3 [UNDIRECTED], radius: 1, diameter: 2
;; Center nodes: (1)
;; Number of nodes: 3 [DIRECTED], radius: 1, diameter: 2
;; Center nodes: (1)
;; Number of nodes: 147 [UNDIRECTED], radius: 3, diameter: 5
;; Center nodes: (16 20 21 24 29 30 33 35)
;; Number of nodes: 147 [DIRECTED], radius: 3, diameter: 6
;; Center nodes: (35)


(process "3
    1 2
    1 3
    2 1" :directed-p nil)

(process "3
    1 2
    1 3
    2 1" :directed-p t)

(process "147
    10 2
    28 2
    2 10
    2 4
    2 29
    2 15
    23 24
    23 29
    15 29
    15 14
    15 34
    7 4
    7 24
    14 2
    14 7
    14 29
    14 11
    14 9
    14 15
    34 15
    34 14
    34 29
    34 24
    34 11
    34 33
    34 20
    29 23
    29 7
    29 2
    29 18
    29 27
    29 4
    29 13
    29 24
    29 11
    29 20
    29 9
    29 34
    29 14
    29 15
    18 27
    18 13
    18 11
    18 29
    27 18
    27 4
    27 24
    4 2
    4 27
    4 13
    4 35
    4 24
    4 20
    4 29
    13 18
    13 16
    13 30
    13 20
    13 29
    13 4
    13 2
    24 4
    24 30
    24 5
    24 19
    24 21
    24 20
    24 11
    24 29
    24 7
    11 18
    11 24
    11 30
    11 33
    11 20
    11 34
    11 14
    20 29
    20 11
    20 4
    20 24
    20 13
    20 33
    20 21
    20 26
    20 22
    20 34
    22 34
    22 11
    22 20
    9 29
    9 20
    21 9
    21 20
    21 19
    21 6
    33 24
    33 35
    33 20
    33 34
    33 14
    33 11
    35 33
    35 4
    35 30
    35 16
    35 19
    35 12
    35 26
    30 13
    30 19
    30 35
    30 11
    30 24
    16 36
    16 19
    16 35
    16 13
    36 16
    31 16
    31 19
    5 19
    19 30
    19 16
    19 5
    19 35
    19 33
    19 24
    12 33
    12 35
    12 3
    12 26
    26 21
    26 35
    6 21
    6 19
    1 6
    8 3
    8 6
    3 8
    3 6
    3 12
    3 35
    33 29
    29 33
    14 33
    29 21" :directed-p nil)

(process "147
    10 2
    28 2
    2 10
    2 4
    2 29
    2 15
    23 24
    23 29
    15 29
    15 14
    15 34
    7 4
    7 24
    14 2
    14 7
    14 29
    14 11
    14 9
    14 15
    34 15
    34 14
    34 29
    34 24
    34 11
    34 33
    34 20
    29 23
    29 7
    29 2
    29 18
    29 27
    29 4
    29 13
    29 24
    29 11
    29 20
    29 9
    29 34
    29 14
    29 15
    18 27
    18 13
    18 11
    18 29
    27 18
    27 4
    27 24
    4 2
    4 27
    4 13
    4 35
    4 24
    4 20
    4 29
    13 18
    13 16
    13 30
    13 20
    13 29
    13 4
    13 2
    24 4
    24 30
    24 5
    24 19
    24 21
    24 20
    24 11
    24 29
    24 7
    11 18
    11 24
    11 30
    11 33
    11 20
    11 34
    11 14
    20 29
    20 11
    20 4
    20 24
    20 13
    20 33
    20 21
    20 26
    20 22
    20 34
    22 34
    22 11
    22 20
    9 29
    9 20
    21 9
    21 20
    21 19
    21 6
    33 24
    33 35
    33 20
    33 34
    33 14
    33 11
    35 33
    35 4
    35 30
    35 16
    35 19
    35 12
    35 26
    30 13
    30 19
    30 35
    30 11
    30 24
    16 36
    16 19
    16 35
    16 13
    36 16
    31 16
    31 19
    5 19
    19 30
    19 16
    19 5
    19 35
    19 33
    19 24
    12 33
    12 35
    12 3
    12 26
    26 21
    26 35
    6 21
    6 19
    1 6
    8 3
    8 6
    3 8
    3 6
    3 12
    3 35
    33 29
    29 33
    14 33
    29 21" :directed-p t)
