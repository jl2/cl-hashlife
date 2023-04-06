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

(defstruct (manual-memoizer (:conc-name mm-))
  (hash-table (make-hash-table :test 'equal
                               ;;#+sbcl :weakness #+sbcl :value
                               ))
  (hash-function (lambda (x) x))
  (hits 0 :type fixnum)
  (misses 0 :type fixnum)
  (enabled nil :type (or t nil)))


(defun mm-hash-table-size (mm)
  (hash-table-count (mm-hash-table mm)))

(defun mm-get (mm &rest params)
  (with-slots (hash-table hash-function enabled hits misses) mm
    (cond
      (enabled
       (multiple-value-bind (val foundp)
           (gethash (apply hash-function params) hash-table nil)
         (if foundp
             (incf hits)
             (incf misses))
         (values val foundp)))

      ((not enabled)
       (values nil nil)))))

(defun mm-calls (mm)
  (with-slots (hits misses) mm
    (+ hits misses)))

(defun mm-reset (mm)
  (with-slots (hash-table hits misses) mm
    (clrhash hash-table)
    (setf hits 0
          misses 0)))

(defun mm-enable (mm)
  (with-slots (enabled) mm
    (setf enabled t)))

(defun mm-disable (mm)
  (with-slots (enabled) mm
    (setf enabled nil)))

(defun mm-add (mm params value)
  (with-slots (hash-table
               hash-function
               enabled)
      mm
    (cond
      (enabled
       (let ((hash-value (apply hash-function params)))
         (setf (gethash
                hash-value
                hash-table)
               value)))
      ((not enabled)
       value))))

(defun mm-hash (mm &rest params)
  (apply (mm-hash-function mm) params))

(defun to-64-bit (num)
  (logand num (- (ash 1 63) 1)))

(defun compute-hash (a b c d)
  (let ((val (to-64-bit (+ (q-k a)
                           2
                           (to-64-bit (* 5131830419411 (q-hash a)))
                           (to-64-bit (* 3758991985019 (q-hash b)))
                           (to-64-bit (* 8973110871315 (q-hash c)))
                           (to-64-bit (* 4318490180473 (q-hash d)))))))
    val))
  ;; (declare (optimize (speed 3) (space 3) (safety 0) (debug 0)))
  ;; (+ (q-k a)
  ;;    2
  ;;    (to-64-bit
  ;;     (logxor
  ;;      (to-64-bit
  ;;       (*
  ;;        (to-64-bit
  ;;         (logxor
  ;;          (to-64-bit
  ;;           (*
  ;;            (to-64-bit
  ;;             (logxor
  ;;              (to-64-bit
  ;;               (* (q-hash a)
  ;;                  5131830419411))
  ;;              (q-hash b)))
  ;;            3758991985019))
  ;;          (q-hash c)))
  ;;        8973110871315))
  ;;      (* ;;4318490180473
  ;;       1
  ;;       (q-hash d))))))


(defparameter *join-memo* (make-manual-memoizer :hash-function #'compute-hash :enabled t))

(defparameter *zero-memo* (make-manual-memoizer :hash-function #'identity :enabled t))

(defparameter *successor-memo* (make-manual-memoizer :hash-function (lambda (node j) (+
                                                                                      (* 10000000 (q-hash node))
                                                                                      j))
                                                     :enabled t))
(defparameter *on* (make-qtnode :k 0 :n 1 :hash 1)
  "Base level binary node 1")

(defparameter *off* (make-qtnode :k 0 :n 0 :hash 0)
  "Base level binary node 0")

(defparameter *mask* (1- (ash 1 63)))


(defun join-table-stats ()
  ;; (let ((cache-hit-rate (if (and (not (zerop *join-cache-misses*))
  ;;                                           (not  (zerop *join-cache-hits*)))
  ;;                                      (/ *join-cache-hits*
  ;;                                         (+ *join-cache-hits* *join-cache-misses*)
  ;;                                         1.0)
  ;;                                      0)))
  ;;   (multiple-value-bind (g10 g20 g50 g100 g500 g1000)
  ;;       (loop :for pt
  ;;               :being :the hash-keys :of *join-table*
  ;;                 :using (hash-value node)
  ;;             :counting (> (q-n node) 10) :into g10
  ;;             :counting (> (q-n node) 20) :into g20
  ;;             :counting (> (q-n node) 50) :into g50
  ;;             :counting (> (q-n node) 100) :into g100
  ;;             :counting (> (q-n node) 500) :into g500
  ;;             :counting (> (q-n node) 1000) :into g1000
  ;;             :finally (return (values g10 g20 g50 g100 g500 g1000)))
  ;;     (format t "Cache hit rate: ~a~%" cache-hit-rate)
  ;;     (format t "Nodes >10 pop : ~a~%" g10)
  ;;     (format t "Nodes >20 pop : ~a~%" g20)
  ;;     (format t "Nodes >50 pop : ~a~%" g50)
  ;;     (format t "Nodes >100 pop : ~a~%" g100)
  ;;     (format t "Nodes >500 pop : ~a~%" g500)
  ;;     (format t "Nodes >1000 pop : ~a~%" g1000)))
  )

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
                 ;;(break)
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
  (let ((z (get-zero (q-k (q-a m)))))
    (with-slots (a b c d) m

      (let ((next (q-join
                   (q-join z z z a)
                   (q-join z z b z)
                   (q-join z c z z)
                   (q-join d z z z))))
        (if (= times 1)
            next
            (center next (- times 1)))))))

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
            (= 2 outer (q-n e)))
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


(defun successor-table-stats ()
  ;; (let ((cache-hit-rate (if (and (not (zerop *successor-cache-misses*))
  ;;                                (not  (zerop *successor-cache-hits*)))
  ;;                           (/ *successor-cache-hits*
  ;;                              (+ *successor-cache-hits* *successor-cache-misses*)
  ;;                              1.0)
  ;;                           0)))
  ;;   (multiple-value-bind (g10 g20 g50 g100 g500 g1000)
  ;;       (loop :for pt
  ;;               :being :the hash-keys :of *successor-table*
  ;;                 :using (hash-value node)
  ;;             :counting (> (q-n node) 10) :into g10
  ;;             :counting (> (q-n node) 20) :into g20
  ;;             :counting (> (q-n node) 50) :into g50
  ;;             :counting (> (q-n node) 100) :into g100
  ;;             :counting (> (q-n node) 500) :into g500
  ;;             :counting (> (q-n node) 1000) :into g1000
  ;;             :finally (return (values g10 g20 g50 g100 g500 g1000)))
  ;;     (format t "Cache hit rate: ~a~%" cache-hit-rate)
  ;;     (format t "Nodes >10 pop : ~a~%" g10)
  ;;     (format t "Nodes >20 pop : ~a~%" g20)
  ;;     (format t "Nodes >50 pop : ~a~%" g50)
  ;;     (format t "Nodes >100 pop : ~a~%" g100)
  ;;     (format t "Nodes >500 pop : ~a~%" g500)
  ;;     (format t "Nodes >1000 pop : ~a~%" g1000)))
  )

(defun inner-successors (m j)
  (with-slots ((m.a a) (m.b b) (m.c c) (m.d d)) m
    (with-slots ((m.a.a a) (m.a.b b) (m.a.c c) (m.a.d d)) m.a
      (with-slots ((m.b.a a) (m.b.b b) (m.b.c c) (m.b.d d)) m.b
        (with-slots ((m.c.a a) (m.c.b b) (m.c.c c) (m.c.d d)) m.c
          (with-slots ((m.d.a a) (m.d.b b) (m.d.c c) (m.d.d d)) m.d
            (let (
                  ;; Top Row
                  (c1 (successor m.a
                                 j))

                  (c2 (successor (q-join m.a.b  m.b.a

                                         m.a.d  m.b.c)
                                 j))
                  (c3 (successor m.b
                                 j))

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
                  (c7 (successor m.c
                                 j))
                  (c8 (successor (q-join m.c.b  m.d.a
                                         m.c.d  m.d.c)
                                 j))
                  (c9 (successor m.d
                                 j)))
              (values c1 c2 c3
                      c4 c5 c6
                      c7 c8 c9))))))))

(defun successor (m &optional (in-j (- (q-k m) 2)))
  "Return the 2**k-1 x 2**k-1 successor, 2**j generations in the future,
where j<= k-2, caching the result."
  (declare (type qtnode m)
           (type (or null integer)  in-j))
  (let* ((j (min in-j (- (q-k m) 2))))
    (when (< j 0)
      (error "j ~a < 0" j))
    (flet ((inner-successor ()
             (cond
               ((zerop (q-n m))
                (q-a m))
               ((= 2 (q-k m))
                (life-4x4 m))
               (t
                (multiple-value-bind (c1 c2 c3
                                      c4 c5 c6
                                      c7 c8 c9)
                    (inner-successors m j)
                  ;;(break)
                  (if (< j (- (q-k m) 2) )
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
      (multiple-value-bind (val found) (mm-get *successor-memo* m j)
        (cond
          (found
           val)

          ((not found)
           (mm-add *successor-memo*
                   (list m in-j)
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
  (let* ((bit-count (1+ (ceiling (log n 2))))
         (new-node (loop
                     :for new-node = (center node) :then (center new-node)
                     :for k :below bit-count
                     :finally (return new-node))))
    (when (null new-node)
      (format t "Warning: new-node is null!~%"))

    ;; (loop :for count :below n
    ;;       :for nod = (successor new-node )
    ;;         :then (successor nod 1)
    ;;       :finally (return nod))
    (loop
      :for k :from 0 :below (1+ bit-count)
      :for bit = (logbitp k n)
      :for j = (- bit-count k 1)
      :for next-node = (if bit
                           (successor new-node j)
                           new-node)
        :then
        (if bit
            (successor next-node j)
            next-node)

      :finally (return next-node))
    ))

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
                 (* size size)
                 1.0)))))

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
        :then (hl::iterate-baseline-life this-pat)
      :for advanced = (hl::advance node i)
      :do
         (format stream "baseline~%===========================================================================================~%")
         (show-life this-pat stream)
         (format stream "~%hashlife~%===========================================================================================~%")
         (show-life advanced stream)
         (format stream "====================================================================================================~%"))))

(defun crop-points (pts lower-left upper-right)
  (loop :for pt :in pts
        :when (pt-in-box pt lower-left upper-right)
          :collect pt))

(defun animate-life (output-directory
                     filename
                     pts-or-node
                     n
                     &key
                       (level 0)
                       (lower-left nil)
                       (upper-right nil)
                       (width 2000)
                       (height 2000))
  (ensure-directories-exist output-directory)
  (loop
    :for i :below n
    :for pts = pts-or-node
      :then (etypecase pts
              (list (baseline-advance pts 1))
              (qtnode (advance pts 1)))

    :for file-name = (format nil "~a/~a~8,'0d.svg" output-directory filename i)
    :do
       (to-svg file-name
               pts
               :level level
               :lower-left lower-left
               :upper-right upper-right
               :width width
               :height height)))

(defun animate-hashlife-qt (output-directory
                            filename
                            node
                            n
                            &key
                              (depth 8)
                              (width 2000))
  (ensure-directories-exist output-directory)
  (loop
    :for i :below n
    :for pts = node
      :then (advance pts 1)

    :for file-name = (format nil "~a/~a~8,'0d.svg" output-directory filename i)
    :do
       (qt-to-svg file-name
                  pts
                  :depth depth
                  :width width)))

(defun animate-hashlife-successor (output-directory
                                   filename
                                   node
                                   n
                                   &key
                                     (depth 8)
                                     (width 2000))
  (ensure-directories-exist output-directory)
  (loop
    :for pts = node
      :then (successor node i)
    :for i :from 0 :below n
    :for file-name = (format nil "~a/~a~8,'0d.svg" output-directory filename i)
    :do
       (qt-to-svg file-name
                  pts
                  :depth depth
                  :width width)))

;; (defun animate-hashlife (output-directory svg-base-name dot-name node iteration-count
;;                          &kjey
;;                          (depth 0)
;;                          (width 2000)
;;                          (stroke-width 1))
;;   (ensure-directories-exist output-directory)
;;   (with-output-to-file (dotf (format nil
;;                                      "~a/svg-base-name.dot"
;;                                      :if-exists :supersede))
;;     (format dotf "digraph {~%")
;;     (loop
;;       :for i :below iteration-count
;;       :for the-node = node
;;         :then (advance the-node 1)
;;       :for svg-name = (format nil "~a/~a~8,'0d.svg" output-directory filename i)
;;       :do
;;          (qt-to-svg svg-base-name)
;;          (format dotf "node~a -> node~a;" )
;;       )))

(defun qt-to-svg (file-name node &key
                                   (depth 0)
                                   (width 2000)
                                   (stroke-width 1))

  (with-output-to-file (outf file-name :if-exists :supersede)
    (svg:with-svg (outf width width :default-stroke-width stroke-width
                                    :default-stroke-color (vec4 0.1 0.1 0.1 0.6)
                                    :view-min (vec2 0 0)
                                    :view-width (vec2 width width))
      (labels ((draw-node (node current-depth min-x max-x min-y max-y)
                 (with-slots (k n a b c d) node
                   (let* ((node-width (- max-x min-x))
                          (node-height (- max-y min-y))
                          (mid-x (+ min-x
                                    (/ node-width
                                       2)))
                          (mid-y (+ min-y
                                    (/ node-height
                                       2)))
                          (size (expt 2 (q-k node)))
                          (gray (- 1 (/ n (* size size) 1.0))))
                     (when (not (zerop n))
                       (svg:rectangle outf
                                      (vec2 min-x min-y)
                                      (vec2 node-width node-height)
                                      :fill-color (vec4 gray gray gray (if (not (zerop n)) 1 0))))
                     (when (and (> (q-k node)
                                   0)
                                (< current-depth depth))
                       (draw-node a
                                  (1+ current-depth)
                                  min-x
                                  mid-x
                                  min-y
                                  mid-y
                                  )
                       (draw-node b
                                  (1+ current-depth)
                                  mid-x
                                  max-x
                                  min-y
                                  mid-y
                                  )
                       (draw-node c
                                  (1+ current-depth)
                                  min-x
                                  mid-x
                                  mid-y
                                  max-y
                                  )
                       (draw-node d
                                  (1+ current-depth)
                                  mid-x
                                  max-x
                                  mid-y
                                  max-y))))))
        (draw-node node 0 0 width 0 width)))))


(defun to-svg (file-name pts-or-node &key
                                       (level 0)
                                       (lower-left nil)
                                       (upper-right nil)
                                       (width 2000)
                                       (height 2000)
                                       (stroke-width 1))
  (let ((remaining-pts (sort (etypecase pts-or-node
                               (list
                                (if (and lower-left upper-right)
                                    (crop-points pts-or-node lower-left upper-right)
                                    pts-or-node))
                               (qtnode
                                (if (and lower-left upper-right)
                                    (expand pts-or-node
                                            :min-x (pt-x lower-left)
                                            :min-y (pt-y lower-left)
                                            :max-x (pt-x upper-right)
                                            :max-y (pt-y upper-right)
                                            :level level)
                                    (expand pts-or-node
                                            :level level))))
                             #'pt-<)))

    (with-output-to-file (outf file-name :if-exists :supersede)

      (svg:with-svg (outf width height :default-stroke-width stroke-width
                                       :view-min (vec2 0 0)
                                       :view-width (vec2 width height))
        (multiple-value-bind (min-x min-y max-x max-y)
            (if (and lower-left upper-right)
                (values (pt-x lower-left) (pt-y lower-left)
                        (pt-x upper-right) (pt-y upper-right))
                (loop :for pt :in remaining-pts
                      :minimizing (pt-x pt) :into min-x
                      :minimizing (pt-y pt) :into min-y
                      :maximizing (pt-x pt) :into max-x
                      :maximizing (pt-y pt) :into max-y
                      :finally (return (values min-x min-y max-x max-y))))
          (let* ((max-cells (max  (1+ (- max-x min-x))
                                  (1+ (- max-y min-y))))
                 (cell-width (/ width max-cells))
                 (cell-height (/ height max-cells))
                 (cell-size (vec2 cell-width cell-height)))
            (flet ((mp (x y) (vec2 (* cell-width
                                      (- (ash x 0) min-x))
                                   (* cell-height
                                      (- (ash y 0) min-y)))))
              (loop
                :for y :from min-y :to (1+ max-y) :do
                  (svg:line outf
                            (mp min-x y)
                            (mp (1+ max-x) y)
                            :stroke-width stroke-width))
              (loop
                :for x :from min-x :to (1+ max-x) :do
                  (svg:line outf
                            (mp x min-y)
                            (mp x (1+ max-y))
                            :stroke-width stroke-width))
              (loop
                :for pt :in remaining-pts
                :do
                   (with-slots (gray x y) pt
                     (svg:rectangle outf
                                    (mp x y)
                                    cell-size
                                    :fill-color (vec4 (- 1 gray)
                                                      (- 1 gray)
                                                      (- 1 gray)
                                                      1.0)))))))))))
