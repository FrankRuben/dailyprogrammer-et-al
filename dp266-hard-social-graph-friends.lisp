(defun find-k-cliques (nodes k k-1-cliques nb-nodes)
  "Return all cliques of length K after extending known cliques K-1-CLIQUES of length K-1 by nodes NB-NODES."
  (declare (type (array bit 2) nodes)
           (type list k-1-cliques)
           (type fixnum k nb-nodes))
  (assert (<= k nb-nodes))

  (labels ((share-edge (from-idx to-idx)
             "Return t iif FROM-IDX and TO-IDX have a common edge."
             (= (aref nodes (1- from-idx) (1- to-idx)) 1))
           (clique-p (clique-edges new-node-idx)
             "Return t iif we'll still have a clique after adding NEW-NODE-IDX to existing clique with CLIQUE-EDGES."
             (every (lambda (clique-node-idx) (share-edge clique-node-idx new-node-idx))
                    clique-edges))
           (collect-extented-cliques (clique-edges)
             "Return a (potentially empty) list of cliques extending CLIQUE-EDGES with one further node."
             (loop for candidate-idx fixnum from 1 upto nb-nodes
                unless (member candidate-idx clique-edges)
                when (clique-p clique-edges candidate-idx)
                collect (cons candidate-idx clique-edges))))

    (let* ((k-cliques (loop for clique-edges in k-1-cliques
                         for extented-cliques = (collect-extented-cliques clique-edges)
                         ;; if extented-cliques is nil here, we simple won't change the appended list
                         append extented-cliques))
           (unique-k-cliques (remove-duplicates
                              ;; sort is destructive, so deep-copy before sorting
                              (mapcar (lambda (l) (sort l #'<)) (mapcar #'copy-seq k-cliques))
                              :test #'equal)))

      (cond
        ((= k nb-nodes)                 ;complete graph checked
         unique-k-cliques)
        ((null unique-k-cliques)        ;no new nodes could be added, so...
         k-1-cliques)                   ;...largest clique size has already been found by caller
        (t                              ;try to add one more node to each existing cliques
         (find-k-cliques nodes (1+ k) unique-k-cliques nb-nodes))))))

(defun process (lines)
  (with-input-from-string (input lines)
    (let* ((n (read input))
           (v (make-array `(,n ,n) :element-type 'bit))
           (k2-cliques '()))
      (loop for i1 = (read input nil)
         while i1
         for i2 = (read input nil)
         do (setf (sbit v (1- i1) (1- i2)) 1)
         do (setf (sbit v (1- i2) (1- i1)) 1)
         do (push (list i1 i2) k2-cliques))
      (let ((largest-cliques (find-k-cliques v 3 k2-cliques n)))
        (format t "For graph of ~d nodes, largest clique~p of size ~d: ~a.~%"
                n (length largest-cliques) (length (car largest-cliques)) largest-cliques)))))

;; See here: https://www.reddit.com/r/dailyprogrammer/comments/4j65ls/20160513_challenge_266_hard_finding_friends_in/
;;
;; Output for sample and challenge:
;;
;; For graph of 7 nodes, largest clique of size 4: ((4 5 6 7)).
;; For graph of 62 nodes, largest cliques of size 5: ((19 25 30 46 52)
;;                                                    (19 22 30 46 52)
;;                                                    (7 10 14 18 58)).

(process "7
1 2
1 3
2 3
1 4
1 6
2 5
2 7
3 4
3 5
4 5
4 7
4 6
5 6
5 7
6 7")

(process "62
11 1
15 1
16 1
41 1
43 1
48 1
18 2
20 2
27 2
28 2
29 2
37 2
42 2
55 2
11 3
43 3
45 3
62 3
9 4
15 4
60 4
52 5
10 6
14 6
57 6
58 6
10 7
14 7
18 7
55 7
57 7
58 7
20 8
28 8
31 8
41 8
55 8
21 9
29 9
38 9
46 9
60 9
14 10
18 10
33 10
42 10
58 10
30 11
43 11
48 11
52 12
34 13
18 14
33 14
42 14
55 14
58 14
17 15
25 15
34 15
35 15
38 15
39 15
41 15
44 15
51 15
53 15
19 16
25 16
41 16
46 16
56 16
60 16
21 17
34 17
38 17
39 17
51 17
23 18
26 18
28 18
32 18
58 18
21 19
22 19
25 19
30 19
46 19
52 19
31 20
55 20
29 21
37 21
39 21
45 21
48 21
51 21
30 22
34 22
38 22
46 22
52 22
37 24
46 24
52 24
30 25
46 25
52 25
27 26
28 26
28 27
31 29
48 29
36 30
44 30
46 30
52 30
53 30
43 31
48 31
61 33
35 34
38 34
39 34
41 34
44 34
51 34
38 35
45 35
50 35
38 37
40 37
41 37
60 37
41 38
44 38
46 38
62 38
44 39
45 39
53 39
59 39
58 40
53 41
55 42
58 42
48 43
51 43
47 44
54 44
51 46
52 46
60 46
50 47
58 49
52 51
56 52
62 54
58 55")
