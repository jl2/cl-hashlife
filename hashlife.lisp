;; hashlife.lisp

;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-hashlife)

(declaim (optimize (speed 1) (space 0) (safety 0) (debug 0) (compilation-speed 0)))
(defun qtnode-hash-func (val)
  (sxhash (q-hash val)))
;;(sb-ext:define-hash-table-test qtnode-hash qtnode-hash-func)

(defmethod cl:print-object ((node qtnode) stream)
  (declare (type stream stream))
  (with-slots (k n a b c d hash) node
    (format stream
            "(qtnode (k ~a) (size ~a) (population ~a) (hash ~a) (~a ~a ~a ~a))"
            k
            (ash 1 k)
            n
            hash
            (if a (q-n a) 0)
            (if b (q-n b) 0)
            (if c (q-n c) 0)
            (if d (q-n d) 0))))


(declaim (inline to-64-bit))
(defun to-64-bit (num)
  (logand num (- (ash 1 63) 1)))

(defun hash-version-1 (a b c d)
  (declare (optimize (speed 1) (space 3) (safety 0) (debug 0))
           (type qtnode a b c d))
  (to-64-bit (+ (q-k a)
                2
                (to-64-bit (* 5131830419411 (q-hash a)))
                (to-64-bit (* 3758991985019 (q-hash b)))
                (to-64-bit (* 8973110871315 (q-hash c)))
                (to-64-bit (* 4318490180473 (q-hash d))))))

(defun hash-version-2 (a b c d)
  (declare (optimize (speed 2) (space 3) (safety 0) (debug 0))
           (type qtnode a b c d))
  (+ (q-k a)
     2
     (to-64-bit
      (logxor
       (to-64-bit
        (*
         (to-64-bit
          (logxor
           (to-64-bit
            (*
             (to-64-bit
              (logxor
               (to-64-bit
                (* (q-hash a)
                   5131830419411))
               (q-hash b)))
             3758991985019))
           (q-hash c)))
         8973110871315))
       (* 4318490180473
          (q-hash d))))))

(defparameter *join-hash-function* #'hash-version-2)
(declaim (type function *join-hash-function* ))
(defun compute-hash (a b c d)
  (declare (optimize (speed 2) (space 3) (safety 0) (debug 0))
           (type qtnode a b c d))
  (funcall *join-hash-function* a b c d))



(defparameter *join-memo* (make-manual-memoizer :hash-function #'compute-hash :enabled t))

(defparameter *zero-memo* (make-manual-memoizer :hash-function #'identity :enabled t))

(defparameter *successor-memo* (make-manual-memoizer :hash-function (lambda (node j) (+
                                                                                      (* 100000 (q-hash node))
                                                                                      j))
                                                     :enabled t))
(defparameter *on* (make-qtnode :k 0 :n 1 :hash 1)
  "Base level binary node 1")

(defparameter *off* (make-qtnode :k 0 :n 0 :hash 0)
  "Base level binary node 0")

(defparameter *mask* (1- (ash 1 63)))


(defun mm-stats (memo)
  (with-slots (hash-table hit-count miss-count hi-count call-count) memo
    (let* (
           (cache-hit-rate (if (and (not (zerop  hit-count))
                                    (not  (zerop call-count)))
                               (/ hit-count
                                  call-count
                                  1.0)
                               0)))
      (multiple-value-bind (g10 g20 g50 g100 g500 g1000)
          (loop :for pt
                  :being :the hash-keys
                    :of hash-table
                      :using (hash-value node)
                :counting (> (q-n node) 10) :into g10
                :counting (> (q-n node) 20) :into g20
                :counting (> (q-n node) 50) :into g50
                :counting (> (q-n node) 100) :into g100
                :counting (> (q-n node) 500) :into g500
                :counting (> (q-n node) 1000) :into g1000
                :finally (return (values g10 g20 g50 g100 g500 g1000)))
        (format t "Cache hit rate: ~a~%" cache-hit-rate)
        (format t "Nodes >10 pop : ~a~%" g10)
        (format t "Nodes >20 pop : ~a~%" g20)
        (format t "Nodes >50 pop : ~a~%" g50)
        (format t "Nodes >100 pop : ~a~%" g100)
        (format t "Nodes >500 pop : ~a~%" g500)
        (format t "Nodes >1000 pop : ~a~%" g1000)))))

(defun q-join (a b c d)
  "Combine four children at level `k-1` to a new node at level `k`.
If this is cached, return the cached node.
Otherwise, create a new node, and add it to the cache."
  (declare (type qtnode a b c d))


  (multiple-value-bind (val found) (mm-get *join-memo* a b c d)
    (cond
      (found
       val)
      ((not found)
       (mm-add *join-memo* (list a b c d)
               (progn
                 (make-qtnode :k (1+ (q-k a))
                              :a a
                              :b b
                              :c c
                              :d d
                              :n (+ (q-n a)
                                    (q-n b)
                                    (q-n c)
                                    (q-n d))
                              :hash (compute-hash a b c d))))))))

(defun get-zero (k)
  "Return an empty node at level `k`."
  (declare (type integer k))

  (multiple-value-bind (val found) (mm-get *zero-memo*  k)
    (cond
      (found
       val)
      ((not found)
       (mm-add *zero-memo*
               (list k)
               (if (> k 0)
                   (let ((omk (- k 1)))
                     (q-join (get-zero omk)
                             (get-zero omk)
                             (get-zero omk)
                             (get-zero omk)))
                   *off*))))))

(defun center (m &optional (times 1))
  "Return a node at level `k+1`, which is centered on the given quadtree node."
  (declare (type qtnode m))
  (loop :for i :to times
        :for z = (get-zero (q-k (q-a m)))
          :then (get-zero (q-k (q-a centered)))
        :for centered = m
          :then
          (with-slots (a b c d) centered
            (q-join
             (q-join z z z a)
             (q-join z z b z)
             (q-join z c z z)
             (q-join d z z z)))
        :finally (return centered)))
(declaim (inline inner-successor ssuccessor q-join life life-4x4))
(defun life (a b c
             d e f
             g h i)
  "The standard life rule, taking eight neighbors and a center cell E.
Returns *on* if should be on, *off* otherwise."
  (declare (type qtnode
                 a b c
                 d e f
                 g h i ))
  (let* ((outer (+ (q-n a)
                   (q-n b)
                   (q-n c)
                   (q-n d)
                   (q-n f)
                   (q-n g)
                   (q-n h)
                   (q-n i))))
    (declare (type integer outer))
    (if (or (= outer 3)
            (and  (= 2 outer)
                  (= 1 (q-n e))))
        *on*
        *off*)))

(defun life-4x4 (m)
  "Return the next generation of a k=2 (i.e. 4x4) cell.
To terminate the recursion at the base level,
if we have a k=2 4x4 block,
we can compute the 2x2 central successor by iterating over all
the 3x3 sub-neighborhoods of 1x1 cells using the standard life rule."
  (declare (type qtnode m))
  (q-join
   ;; Copy/pasted from. .(find-file-other-window "~/src/hashlife/hashlife.py" )
   ;; na = life(m.a.a, m.a.b, m.b.a, m.a.c, m.a.d, m.b.c, m.c.a, m.c.b, m.d.a)  # AD
   (life        (q-a (q-a m))  (q-b (q-a m))  (q-a (q-b m))
                (q-c (q-a m))  (q-d (q-a m))  (q-c (q-b m))
                (q-a (q-c m))  (q-b (q-c m))  (q-a (q-d m)))

   ;; nb = life((q-b (q-a m)), (q-a (q-b m)), (q-b (q-b m)), (q-d (q-a m)), (q-c (q-b m)), (q-d (q-b m)), (q-b (q-c m)), (q-a (q-d m)), (q-b (q-d m)))  # BC
   (life        (q-b (q-a m))  (q-a (q-b m))  (q-b (q-b m))
                (q-d (q-a m))  (q-c (q-b m))  (q-d (q-b m))
                (q-b (q-c m))  (q-a (q-d m))  (q-b (q-d m)))

   ;; nc = life(m.a.c, m.a.d, m.b.c, m.c.a, m.c.b, m.d.a, m.c.c, m.c.d, m.d.c)  # CB
   (life        (q-c (q-a m))  (q-d (q-a m))  (q-c (q-b m))
                (q-a (q-c m))  (q-b (q-c m))  (q-a (q-d m))
                (q-c (q-c m))  (q-d (q-c m))  (q-c (q-d m)))

   ;; nd = life(m.a.d, m.b.c, m.b.d, m.c.b, m.d.a, m.d.b, m.c.d, m.d.c, m.d.d)  # DA
   (life        (q-d (q-a m))  (q-c (q-b m))  (q-d (q-b m))
                (q-b (q-c m))  (q-a (q-d m))  (q-b (q-d m))
                (q-d (q-c m))  (q-c (q-d m))  (q-d (q-d m)))))

(defun inner-successors (m j)
  (let (
        ;; Top Row
        (c1 (successor (q-a m)
                       j))

        (c2 (successor (q-join (q-b (q-a m))  (q-a (q-b m))

                               (q-d (q-a m))  (q-c (q-b m)))
                       j))
        (c3 (successor (q-b m)
                       j))

        ;; Middle Row
        (c4 (successor (q-join (q-c (q-a m))  (q-d (q-a m))
                               (q-a (q-c m))  (q-b (q-c m)))
                       j))
        (c5 (successor (q-join (q-d (q-a m))  (q-c (q-b m))
                               (q-b (q-c m))  (q-a (q-d m)))
                       j))
        (c6 (successor (q-join (q-c (q-b m))  (q-d (q-b m))
                               (q-a (q-d m))  (q-b (q-d m)))
                       j))

        ;; Bottom row
        (c7 (successor (q-c m)
                       j))
        (c8 (successor (q-join (q-b (q-c m))  (q-a (q-d m))
                               (q-d (q-c m))  (q-c (q-d m)))
                       j))
        (c9 (successor (q-d m)
                       j)))

    ;; (format t "m ~a~%" m)
    ;; (format t "(q-a m) ~a~%(q-b m) ~a~%(q-c m) ~a~%(q-d m) ~a~%" (q-a m) (q-b m) (q-c m) (q-d m) )
    ;; (format t "(q-a (q-a m)) ~a~%(q-b (q-a m)) ~a~%(q-c (q-a m)) ~a~%(q-d (q-a m)) ~a~%" (q-a (q-a m)) (q-b (q-a m)) (q-c (q-a m)) (q-d (q-a m)))
    ;; (format t "(q-a (q-b m)) ~a~%(q-b (q-b m)) ~a~%(q-c (q-b m)) ~a~%(q-d (q-b m)) ~a~%" (q-a (q-b m)) (q-b (q-b m)) (q-c (q-b m)) (q-d (q-b m)))
    ;; (format t "(q-a (q-c m)) ~a~%(q-b (q-c m)) ~a~%(q-c (q-c m)) ~a~%(q-d (q-c m)) ~a~%" (q-a (q-c m)) (q-b (q-c m)) (q-c (q-c m)) (q-d (q-c m)))
    ;; (format t "(q-a (q-d m)) ~a~%(q-b (q-d m)) ~a~%(q-c (q-d m)) ~a~%(q-d (q-d m)) ~a~%" (q-a (q-d m)) (q-b (q-d m)) (q-c (q-d m)) (q-d (q-d m)))
    ;; (format t "c1 ~a~%c2 ~a~%c3 ~a~%c4 ~a~%c5 ~a~%c6 ~a~%c7 ~a~%c8 ~a~%c9 ~a~%"
    ;;         c1 c2 c3 c4 c5 c6 c7 c8 c9)


    (values c1 c2 c3
            c4 c5 c6
            c7 c8 c9)))

(defun successor (node &optional (in-j (- (q-k node) 2)))
  "Return the 2**k-1 x 2**k-1 successor, 2**j generations in the future,
where j<= k-2, caching the result."
  (declare (type qtnode node)
           (type (or null integer)  in-j))
  (let* ((j (min in-j (- (q-k node) 2))))
    (when (< j 0)
      (error "j ~a < 0" j))
    (flet
        ((inner-successor ()
           (cond
             ((zerop (q-n node))
              (q-a node))

             ((= 2 (q-k node))
              (life-4x4 node))

             (t
              (multiple-value-bind (c1 c2 c3
                                    c4 c5 c6
                                    c7 c8 c9)
                  (inner-successors node j)
                ;;(break)
                (if (< j (- (q-k node) 2) )
                    (q-join
                     (q-join (q-d c1) (q-c c2)
                             (q-b c4) (q-a c5))
                     (q-join (q-d c2) (q-c c3)
                             (q-b c5) (q-a c6))

                     (q-join (q-d c4) (q-c c5)
                             (q-b c7) (q-a c8))
                     (q-join (q-d c5) (q-c c6)
                             (q-b c8) (q-a c9)))
                    (q-join
                     (successor (q-join c1 c2
                                        c4 c5)
                                j)
                     (successor (q-join c2 c3
                                        c5 c6)
                                j)
                     (successor (q-join c4 c5
                                        c7 c8)
                                j)
                     (successor (q-join c5 c6
                                        c8 c9)
                                j))))))))
      (multiple-value-bind (val found) (mm-get *successor-memo* node j)
        (cond
          (found
           val)

          ((not found)
           (mm-add *successor-memo*
                   (list node in-j)
                   (inner-successor))))))))

(defun construct (pts)
  "Turn a list of x y coordinates into a quadtree and return the top level node."

  (let* ((min-pt (loop
                   :for pt :in pts
                   :minimizing (pt-x pt) :into min-x
                   :minimizing (pt-y pt) :into min-y
                   :finally (return (pt min-x min-y *on*))))
         (pattern (mapcar
                   (lambda (pt)
                     (pt (- (pt-x pt) (pt-x min-pt))
                         (- (pt-y pt) (pt-y min-pt))
                         *on*))
                   pts)))
    (pad
     (loop
       :while (/= 1 (length pattern))
       :for k :from 0
       :for z = (get-zero k)
       :for next-pattern
         = (loop
             :while (> (length pattern)
                       0)

             :for pt = (car pattern)

             :for ept = (even-pt pt)

             :for a = (find ept
                            pattern
                            :test #'pt-=)

             :for b = (find (right-pt ept)
                            pattern
                            :test #'pt-=)

             :for c = (find (down-pt ept)
                            pattern
                            :test #'pt-=)

             :for d = (find (corner-pt ept)
                            pattern
                            :test #'pt-=)

             :collecting (pt (ash (pt-x ept) -1)
                             (ash (pt-y ept) -1)
                             (q-join (if a
                                         (pt-gray a)
                                         z)
                                     (if b
                                         (pt-gray b)
                                         z)
                                     (if c
                                         (pt-gray c)
                                         z)
                                     (if d
                                         (pt-gray d)
                                         z)))
               :into next-level
             :when a
               :do (setf pattern
                         (remove a pattern :test #'pt-=))
             :when b
               :do (setf pattern
                         (remove b pattern :test #'pt-=))
             :when c
               :do (setf pattern
                         (remove c pattern :test #'pt-=))
             :when d
               :do (setf pattern
                         (remove d pattern :test #'pt-=))
             :finally (return next-level))
       :do
          (setf pattern next-pattern)
          ;; Return quadtree node from ( (x . y) . qtnode) pair
       :finally (return (pt-gray (car pattern)))))))

(defun inner (node)
  (q-join (q-d (q-a node))
          (q-c (q-b node))
          (q-b (q-c node))
          (q-a (q-d node))))

(defun crop (node)
  (cond ((or (<= (q-k node) 3 )
             (not (is-padded node)))
         node)
        (t
         (crop (inner node)))))

(defun ffwd (node n)
  (loop
    :for padded-node = (pad node)
      :then (pad next-node)
    :for next-node = (successor padded-node)
    :for gens = 0
      :then (+ gens (ash 1 (- (q-k padded-node) 2)))
    :for i :below n
    :finally (return (values next-node gens))))

(defun advance (node n)
  "Advance node by exactly n generations, using the binary
expansion of n to find the correct successors."
  (declare (ignorable node n))
  (when (= 0 n)
    (return-from advance node))
  (let* ((bit-count (1+ (ceiling (log n 2))))
         (new-node (loop
                     :for new-node = (center node)
                       :then (center new-node)
                     :for k :below bit-count
                     :finally (return new-node))))
    (when (null new-node)
      (format t "Warning: new-node is null!~%"))

    ;; (loop :for count :below n
    ;;       :for nod = (successor new-node )
    ;;         :then (successor nod 1)
    ;;       :finally (return nod))

    (loop
      :for k :from 0 :to (1+ bit-count)
      :for bit = (logbitp k n)
      :for j = (- bit-count k 1)
      :for next-node = new-node
        :then
        (if bit
            (successor next-node j)
            next-node)
      :finally
         (return (crop next-node)))))

(defun is-padded (node)
  (with-slots (a b c d) node
    (and ;; (q-a node)
     (= (q-n a)
        (q-n (q-d (q-d a))))

     (= (q-n b)
        (q-n (q-c (q-c b))))

     (= (q-n c)
        (q-n (q-b (q-b c))))

     (= (q-n d)
        (q-n (q-a (q-a d)))))))

(defun pad (node)
  (if (or (<= (q-k node) 3)
          (not  (is-padded node)))
      (pad (center node))
      node))

(defun expand (node &key
                      (x 0) (y 0)
                      min-x max-x
                      min-y max-y
                      (level 0))
  "Turn a quadtree into a list of (x, y, gray) triples in
the rectangle (x,y) -> (lower-bound - upper-bound)"
  ;; Null or no cells on means skip over this whole quadtree
  (when (or (null node)
            (= 0 (q-n node)))
    (return-from expand nil))

  (let* ((size (expt 2 (q-k node)))
         (offset (ash size -1)))
    (cond

      ;; Clipping outside of min-x, max-x and min-y, max-y
      ((and min-x
            min-y
            max-x
            max-y
            (or
             (< x  min-x)
             (< y  min-y)
             (> x  max-x)
             (> y  max-y)))
       nil)

      ;; Collect cells at this zoom level
      ((= (q-k node) level)

       ;; Scale quadtree to match scale of level 0
       (let ((reduction (- level)))
         (list
          (pt (ash x reduction)
              (ash y reduction)
              (/ (q-n node)
                 (* size size))))))

      ;; Otherwise try expanding the children
      (t
       (with-slots (a b c d) node
         (concatenate
          'list
          (expand a
                  :x x
                  :y y
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)
          (expand b
                  :x (+ x offset)
                  :y y
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)
          (expand c
                  :x x
                  :y (+ y offset)
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)
          (expand d
                  :x (+ x offset)
                  :y (+ y offset)
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)))))))

(defun align (pts)
  (let* ((min-pt (loop
                   :for pt :in pts
                   :minimizing (pt-x pt) :into min-x
                   :minimizing (pt-y pt) :into min-y
                   :finally (return (pt min-x min-y *on*)))))
    (sort (mapcar
           (lambda (pt)
             (hl::pt (- (pt-x pt) (pt-x min-pt))
                     (- (pt-y pt) (pt-y min-pt))
                     1))
           pts)
          #'pt-<)))

(defun same-pattern (pts expanded)
  (every #'hl::pt-= (align pts) (align expanded)))

(defun show-side-by-side (pat-name n &optional (stream t))
  (declare (type fixnum n))
  (let* ((pat (read-game-file pat-name))
         (node (construct pat)))
    (loop
      :for i fixnum :below n
      :for this-pat = pat
        :then (baseline-advance pat i)
      :for this-node = node
        :then (advance node i)
      :do
         (format stream "baseline~%===========================================================================================~%")
         (show-life this-pat stream)
         (format stream "~%hashlife~%===========================================================================================~%")
         (show-life this-node stream)
         (format stream "====================================================================================================~%"))))

(defun crop-points (pts lower-left upper-right)
  (loop :for pt :in pts
        :when (pt-in-box pt lower-left upper-right)
          :collect pt))

(defun testpat2 ()
  (q-join
   (q-join (q-join *on* *on*
                   *on* *on*)
           (q-join *off* *off*
                   *off* *off*)
           (q-join *off* *off*
                   *off* *off*)
           (q-join *off* *off*
                   *off* *off*))
   (q-join (q-join *off* *off*
                   *off* *on*)
           (q-join *on* *off*
                   *off* *off*)
           (q-join *off* *on*
                   *off* *off*)
           (q-join *on* *on*
                   *off* *off*))

   (q-join (q-join *off* *off*
                   *on* *on*)
           (q-join *off* *off*
                   *on* *off*)
           (q-join *off* *off*
                   *off* *off*)
           (q-join *off* *off*
                   *off* *off*))
   (q-join (q-join *off* *off*
                   *off* *on*)
           (q-join *off* *off*
                   *on* *off*)
           (q-join *off* *on*
                   *off* *off*)
           (q-join *off* *on*
                   *on* *on*))))
