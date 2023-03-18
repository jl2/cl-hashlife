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




(defun qtnode-hash-func (val)
  (sxhash (q-hash val)))
;;(sb-ext:define-hash-table-test qtnode-hash qtnode-hash-func)

(defmethod cl:print-object ((node qtnode) stream)
  (with-slots (k n a b c d hash) node
    (format stream "(qtnode (k ~a) (size ~a) (population ~a) (hash ~a) (~a ~a ~a ~a))"
            k
            (ash 1 k)
            n
            hash
            (if a (q-n a) 0)
            (if b (q-n b) 0)
            (if c (q-n c) 0)
            (if d (q-n d) 0))))

(defparameter *on* (make-qtnode :k 0 :n 1 :hash 1)
  "Base level binary node 1")

(defparameter *off* (make-qtnode :k 0 :n 0 :hash 0)
  "Base level binary node 0")

(defparameter *mask* (1- (ash 1 63)))

(defun to-64-bit (num)
  (logand num (1-  (ash 1 62))))

(defun compute-hash (a b c d)
  ;; (let ((val (logand (1- (ash 1 63))
  ;;                    (+ (q-k a)
  ;;                       2
  ;;                       (* 5131830419411 (q-hash a))
  ;;                       (* 3758991985019 (q-hash b))
  ;;                       (* 8973110871315 (q-hash c))
  ;;                       (* 4318490180473 (q-hash d))))))
  ;;   val)
  (declare (optimize (speed 3) (space 3) (safety 0) (debug 0)))
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
                   4318490180473))
               (q-hash b)))
             3758991985019))
           (q-hash c)))
         8973110871315))
       (q-hash d)))))

(defparameter *join-table* (make-hash-table :test 'eql))
(defparameter *join-calls* nil)
(defun q-join (a b c d)
  "Combine four children at level `k-1` to a new node at level `k`.
If this is cached, return the cached node.
Otherwise, create a new node, and add it to the cache."
  (declare (type qtnode a b c d))

  (let ((rval (make-qtnode :k (1+ (q-k a))
                           :a a
                           :b b
                           :c c
                           :d d
                           :n (+ (q-n a)
                                 (q-n b)
                                 (q-n c)
                                 (q-n d))
                           :hash (compute-hash a b c d))))
    (setf (gethash (compute-hash a b c d) *join-table*) rval)
    rval)

  ;; (let ((the-hash (compute-hash a b c d)))
  ;;   (multiple-value-bind (val found) (gethash the-hash *join-table*)
  ;;     (declare (ignorable val))
  ;;     (when (not found)
  ;;       (setf (gethash the-hash *join-table*)
  ;;             (make-qtnode :k (1+ (q-k a))
  ;;                          :a a
  ;;                          :b b
  ;;                          :c c
  ;;                          :d d
  ;;                          :n (+ (q-n a)
  ;;                                (q-n b)
  ;;                                (q-n c)
  ;;                                (q-n d))
  ;;                          :hash the-hash))
  ;;       )
  ;;     (push (list the-hash a b c d) *join-calls*)
  ;;     (gethash the-hash *join-table*)))
  )


;;(fare-memoization:memoize 'get-zero)
(defparameter *zero-table* (make-hash-table :test 'equal))
(defun get-zero (k)
  "Return an empty node at level `k`."
  (declare (type integer k))
  (multiple-value-bind (val found) (gethash k *zero-table*)
    (cond (found
           val)
          (t
           (setf
            (gethash k *zero-table*)
            (if (> k 0)
                (let ((omk (- k 1)))
                  (q-join (get-zero omk)
                          (get-zero omk)
                          (get-zero omk)
                          (get-zero omk)))
                *off*))
           (gethash k *zero-table*)))))

(defun center (m)
  "Return a node at level `k+1`, which is centered on the given quadtree node."
  (declare (type qtnode m))
  (let ((z (get-zero (q-k (q-a m)))))
   (with-slots (a b c d) m
      (q-join
       (q-join z z z a)
       (q-join z z b z)
       (q-join z c z z)
       (q-join d z z z)))))

(defun life (a b c
             d e f
             g h i)
  "The standard life rule, taking eight neighbors and a center cell E.
Returns *on* if should be on, *off* otherwise."
  (declare (type qtnode
                 a b c
                 d e f
                 g h i ))
  (let* ((outer (loop :for node
                           :in (list a b c
                                     d   f
                                     g h i)
                         :summing (q-n node))))
    (declare (type integer outer))
    (if (or (= outer 3)
            (= 2 outer(q-n e)))
        *on*
        *off*)))

(defun life-4x4 (m)
  "Return the next generation of a k=2 (i.e. 4x4) cell.
To terminate the recursion at the base level,
if we have a k=2 4x4 block,
we can compute the 2x2 central successor by iterating over all
the 3x3 sub-neighborhoods of 1x1 cells using the standard life rule."
  (declare (type qtnode m))
  (with-slots ((ma a) (mb b) (mc c) (md d)) m
    (with-slots ((m.a.a a) (m.a.b b) (m.a.c c) (m.a.d d)) ma
      (with-slots ((m.b.a a) (m.b.b b) (m.b.c c) (m.b.d d)) mb
        (with-slots ((m.c.a a) (m.c.b b) (m.c.c c) (m.c.d d)) mc
          (with-slots ((m.d.a a) (m.d.b b) (m.d.c c) (m.d.d d)) md
            (q-join
             ;; Copy/pasted from. .(find-file-other-window "~/src/hashlife/hashlife.py" )
             ;; na = life(m.a.a, m.a.b, m.b.a, m.a.c, m.a.d, m.b.c, m.c.a, m.c.b, m.d.a)  # AD
             (life        m.a.a  m.a.b  m.b.a
                          m.a.c  m.a.d  m.b.c
                          m.c.a  m.c.b  m.d.a)

             ;; nb = life(m.a.b, m.b.a, m.b.b, m.a.d, m.b.c, m.b.d, m.c.b, m.d.a, m.d.b)  # BC
             (life        m.a.b  m.b.a  m.b.b
                          m.a.d  m.b.c  m.b.d
                          m.c.b  m.d.a  m.d.b)

             ;; nc = life(m.a.c, m.a.d, m.b.c, m.c.a, m.c.b, m.d.a, m.c.c, m.c.d, m.d.c)  # CB
             (life        m.a.c  m.a.d  m.b.c
                          m.c.a  m.c.b  m.d.a
                          m.c.c  m.c.d  m.d.c)

             ;; nd = life(m.a.d, m.b.c, m.b.d, m.c.b, m.d.a, m.d.b, m.c.d, m.d.c, m.d.d)  # DA
             (life        m.a.d  m.b.c  m.b.d
                          m.c.b  m.d.a  m.d.b
                          m.c.d  m.d.c  m.d.d))))))))



(defparameter *successor-table* (make-hash-table :test 'equal))

(defun successor (m &optional (j (- (q-k m) 2)))
  "Return the 2**k-1 x 2**k-1 successor, 2**j generations in the future,
where j<= k-2, caching the result."
  (declare (type qtnode m)
           (type (or integer)  j))

  (let* ((real-j (min j (- (q-k m) 2)))
         (the-hash (cons (q-hash m) real-j)))
    ;;    (format t "m: ~a - hash: ~a~%" m the-hash)
    (multiple-value-bind (val found) (gethash the-hash *successor-table*)
      (cond (found
;;                         (format t "Cached: ~a~%" the-hash)
             val)
            (t
             (setf (gethash the-hash *successor-table*)
                   (cond
                     ((= (q-n m) 0)
                      (q-a m))
                     ((= (q-k m) 2)
                      (life-4x4 m))
                     (t
                      (with-slots ((m.a a) (m.b b) (m.c c) (m.d d)) m
                        (with-slots ((m.a.a a) (m.a.b b) (m.a.c c) (m.a.d d)) m.a
                          (with-slots ((m.b.a a) (m.b.b b) (m.b.c c) (m.b.d d)) m.b
                            (with-slots ((m.c.a a) (m.c.b b) (m.c.c c) (m.c.d d)) m.c
                              (with-slots ((m.d.a a) (m.d.b b) (m.d.c c) (m.d.d d)) m.d
                                (let ((j (min j (- (q-k m) 2))))
                                  (let (
                                        ;; Top Row
                                        (c1 (successor (q-join m.a.a m.a.b
                                                               m.a.c m.a.d)
                                                       j))

                                        (c2 (successor (q-join m.a.b  m.b.a
                                                               m.a.d  m.b.c)
                                                       j))
                                        (c3 (successor m.b j))

                                        ;; Middle Row
                                        (c4 (successor (q-join m.a.c  m.a.d
                                                               m.c.a  m.c.b)
                                                       j))
                                        (c5 (successor (q-join m.a.d  m.b.c
                                                               m.c.b  m.d.a)
                                                       j))
                                        (c6 (successor (q-join m.b.c  m.b.d
                                                               m.d.a  m.d.b)
                                                       j))

                                        ;; Bottom row
                                        (c7 (successor (q-join m.c.a m.c.b
                                                               m.c.c m.c.d)
                                                       j))
                                        (c8 (successor (q-join m.c.b  m.d.a
                                                               m.c.d  m.d.c)
                                                       j))
                                        (c9 (successor m.d j)))

                                    (if (< j (- (q-k m) 2) )
                                        (progn
                                          (q-join
                                           (q-join (q-d c1) (q-c c2)
                                                   (q-b c4) (q-a c5))
                                           (q-join (q-d c2) (q-c c3)
                                                   (q-b c5) (q-a c6))

                                           (q-join (q-d c4) (q-c c5)
                                                   (q-b c7) (q-a c8))
                                           (q-join (q-d c5) (q-c c6)
                                                   (q-b c8) (q-a c9))))
                                        (progn
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
                                                      j))))))))))))))
             (gethash the-hash *successor-table*))))))



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
             :do
                (when a
                  (setf pattern
                        (remove a pattern :test #'pt-=)))
                (when b
                  (setf pattern
                        (remove b pattern :test #'pt-=)))
                (when c
                  (setf pattern
                        (remove c pattern :test #'pt-=)))
                (when d
                  (setf pattern
                        (remove d pattern :test #'pt-=)))
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

  (multiple-value-bind (bits new-node)
      (loop
        :for nn = n :then (ash nn -1)
        :for new-node = node
          :then (center node)
        :while (> nn 0 )
        :collecting (logand 1 nn)
          :into bits
        :finally (return (values bits new-node)))
    (crop
     (loop
       :with len = (length bits)
       :for k :from 0
       :for j = (- len k 1)
       :for bit :in (reverse bits)
       :for node = new-node
         :then (if (= bit 1)
                   (successor node j)
                   node)


       :finally (return node)))))

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
  (if (and (< 3 (q-k node))
          (not (is-padded node)))
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

  (let* ((size (expt 2 (q-k node) ))
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
       (let ((reduction (-
                         (+ level  ;;  Higher levels cover more of the quadtree
                            offset
                            ))))
         (list
          (pt (ash x reduction)
              (ash y reduction)
              (/ (q-n node)
                 (* size size))))))

      ;; Otherwise try expanding the children
      (t
       (with-slots (a b c d) node
         ;; Merge sort
         (merge 'list
                (merge 'list
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
                       #'pt-<)
                (merge 'list


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
                               :level level)
                       #'pt-<)
                #'pt-<))))))
