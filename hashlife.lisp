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

(deftype maybe (thing)
  "A thing or null."
  `(or null ,thing))

(defstruct (qtnode (:conc-name q-))
  "A quad tree node"
  (level 1 :type fixnum)
  (nw nil :type (maybe qtnode))
  (ne nil :type (maybe qtnode))
  (sw nil :type (maybe qtnode))
  (se nil :type (maybe qtnode))
  (n 0 :type integer)
  (hash 0 :type integer)
  (next-generation nil :type (maybe qtnode)))

(defun node-size (node)
  "Width/height in cells of a Quadtree node."
  (ash 1 (q-level node)))

(defun set-bit (node x y)
  (with-slots (level nw ne sw se) node
    (let ((offset (ash 1 (- level 2))))
      (cond ((zerop level)
             *on*)

            ((and (< x 0)
                  (< y 0))
             (q-join (set-bit nw
                              (+ x offset)
                              (+ y offset))
                     ne
                     sw
                     se))

            ((< x 0)
             (q-join nw
                     ne
                     (set-bit sw
                              (+ x offset)
                              (- y offset))
                     se))

            ((< y 0)
             (q-join nw
                     (set-bit ne
                              (- x offset)
                              (+ y offset))
                     sw
                     se))
            ((< x 0)
             (q-join nw
                     ne
                     sw
                     (set-bit se
                              (- x offset)
                              (- y offset))))))))

(defun get-bit (node x y)
  (with-slots (level nw ne sw se) node
    (let ((offset (ash 1 (- level 2))))
      (cond ((zerop level)
             1)

            ((and (< x 0)
                  (< y 0))
             (get-bit nw
                      (+ x offset)
                      (+ y offset)))

            ((< x 0)
             (get-bit ne
                      (+ x offset)
                      (- y offset)))

            ((< y 0)
             (get-bit sw
                      (- x offset)
                      (+ y offset)))
            ((< x 0)
             (get-bit se
                      (- x offset)
                      (- y offset)))))))

;;(sb-ext:define-hash-table-test qtnode-hash qtnode-hash-func)

(defmethod cl:print-object ((node qtnode) stream)
  (declare (type stream stream))
  (with-slots (level n nw ne sw se hash) node
    (format stream
            "(qtnode (level ~a) (size ~a) (population ~a) (hash ~a) (~a ~a ~a ~a))"
            level
            (ash 1 level)
            n
            hash
            (if nw
                (q-n nw)
                0)
            (if ne
                (q-n ne)
                0)
            (if sw
                (q-n sw)
                0)
            (if se
                (q-n se)
                0))))

(declaim (inline to-64-bit)
         (ftype (function ((unsigned-byte 63)) (unsigned-byte 63)) to-64-bit))

(defun to-64-bit (num)
  (logand num (- (ash 1 63) 1)))

(defun hash-version-1 (nw ne sw se)
  (declare (optimize (speed 1) (space 3) (safety 0) (debug 0))
           (type qtnode nw ne sw se))
  (to-64-bit (+ (q-level nw)
                2
                (to-64-bit (* 5131830419411 (q-hash nw)))
                (to-64-bit (* 3758991985019 (q-hash ne)))
                (to-64-bit (* 8973110871315 (q-hash sw)))
                (to-64-bit (* 4318490180473 (q-hash se))))))

(defun hash-version-2 (nw ne sw se)
  (declare (optimize (speed 1) (space 3) (safety 0) (debug 0))
           (type qtnode nw ne sw se))
  (+ (q-level nw)
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
                (* (q-hash nw)
                   (coerce 5131830419411 '(unsigned-byte 63))))
               (q-hash ne)))
             3758991985019))
           (q-hash sw)))
         8973110871315))
       (* 4318490180473
          (q-hash se))))))

(defun hash-version-3 (nw ne sw se)
  (declare (optimize (speed 2) (space 3) (safety 0) (debug 0))
           (type qtnode nw ne sw se))
  (+ (q-level nw)
     2
     (logxor
      (*
       (logxor
        (*
         (logxor
          (* (q-hash nw)
             5131830419411)
          (q-hash ne))
         3758991985019)
        (q-hash sw))
       8973110871315)
      (* 4318490180473
         (q-hash se)))))

(defun hash-version-4 (nw ne sw se)
  (declare (optimize (speed 1) (space 3) (safety 0) (debug 0))
           (type qtnode nw ne sw se))
  (+ (q-hash nw)
     (* 11 (q-hash ne))
     (* 101 (q-hash sw))
     (* 1007 (q-hash se))))

(defparameter *join-hash-function* #'hash-version-4)
(declaim (type function *join-hash-function* ))
(defun compute-hash (nw ne sw se)
  (declare (optimize (speed 2) (space 3) (safety 0) (debug 0))
           (type qtnode nw ne sw se))
  (funcall *join-hash-function* nw ne sw se))



(defparameter *join-memo* (make-manual-memoizer :hash-function #'compute-hash :enabled t))

(defparameter *zero-memo* (make-manual-memoizer :hash-function #'identity :enabled t))

(defparameter *successor-memo* (make-manual-memoizer :hash-function (lambda (node j) (+
                                                                                      (* 100000 (q-hash node))
                                                                                      j))
                                                     :enabled t))
(defparameter *next-gen-memo* (make-manual-memoizer
                               :hash-function #'q-hash
                               :enabled nil))
(defun mm-reset-all ()
  (mm-reset *join-memo*)
  (mm-reset *successor-memo*)
  (mm-reset *zero-memo*))

(defparameter *on* (make-qtnode :level 0 :n 1 :hash 1)
  "Base level binary node 1")

(defparameter *off* (make-qtnode :level 0 :n 0 :hash 0)
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

(defun q-join (nw ne sw se)
  "Combine four children at level `k-1` to a new node at level `k`.
If this is cached, return the cached node.
Otherwise, create a new node, and add it to the cache."
  (declare (type qtnode nw ne sw se))


  (multiple-value-bind (val found) (mm-get *join-memo* nw ne sw se)
    (cond
      (found
       val)
      ((not found)
       (mm-add *join-memo* (list nw ne sw se)
               (progn
                 (make-qtnode :level (1+ (q-level nw))
                              :nw nw
                              :ne ne
                              :sw sw
                              :se se
                              :n (+ (q-n nw)
                                    (q-n ne)
                                    (q-n sw)
                                    (q-n se))
                              :hash (compute-hash nw ne sw se))))))))

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
        :for z = (get-zero (q-level (q-nw m)))
          :then (get-zero (q-level (q-nw centered)))
        :for centered = m
          :then
          (with-slots (nw ne sw se) centered
            (q-join
             (q-join z z z nw)
             (q-join z z ne z)
             (q-join z sw z z)
             (q-join se z z z)))
        :finally (return centered)))


;; (declaim (inline inner-successor
;;                  successor
;;                  q-join
;;                  life
;;                  life-4x4))

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
   (life        (q-nw (q-nw m))  (q-ne (q-nw m))  (q-nw (q-ne m))
                (q-sw (q-nw m))  (q-se (q-nw m))  (q-sw (q-ne m))
                (q-nw (q-sw m))  (q-ne (q-sw m))  (q-nw (q-se m)))

   ;; nb = life((q-ne (q-nw m)), (q-nw (q-ne m)), (q-ne (q-ne m)), (q-se (q-nw m)), (q-sw (q-ne m)), (q-se (q-ne m)), (q-ne (q-sw m)), (q-nw (q-se m)), (q-ne (q-se m)))  # BC
   (life        (q-ne (q-nw m))  (q-nw (q-ne m))  (q-ne (q-ne m))
                (q-se (q-nw m))  (q-sw (q-ne m))  (q-se (q-ne m))
                (q-ne (q-sw m))  (q-nw (q-se m))  (q-ne (q-se m)))

   ;; nc = life(m.a.c, m.a.d, m.b.c, m.c.a, m.c.b, m.d.a, m.c.c, m.c.d, m.d.c)  # CB
   (life        (q-sw (q-nw m))  (q-se (q-nw m))  (q-sw (q-ne m))
                (q-nw (q-sw m))  (q-ne (q-sw m))  (q-nw (q-se m))
                (q-sw (q-sw m))  (q-se (q-sw m))  (q-sw (q-se m)))

   ;; nd = life(m.a.d, m.b.c, m.b.d, m.c.b, m.d.a, m.d.b, m.c.d, m.d.c, m.d.d)  # DA
   (life        (q-se (q-nw m))  (q-sw (q-ne m))  (q-se (q-ne m))
                (q-ne (q-sw m))  (q-nw (q-se m))  (q-ne (q-se m))
                (q-se (q-sw m))  (q-sw (q-se m))  (q-se (q-se m)))))

(defun horizontal-forward (w e)
  (next-generation (q-join (q-ne w)
                           (q-nw e)
                           (q-se w)
                           (q-sw e))))

(defun vertical-forward (n s)
  (next-generation (q-join (q-sw n)
                           (q-se n)
                           (q-nw s)
                           (q-ne s))))

(defun center-forward (node)
  (with-slots (nw ne sw se) node
    (next-generation (q-join (q-se nw)
                             (q-sw ne)
                             (q-ne sw)
                             (q-nw se)))))


(defun centered-sub-node (node)
  (with-slots (nw ne sw se) node
    (q-join (q-se nw)
            (q-sw ne)
            (q-ne sw)
            (q-nw se))))

(defun centered-horizontal (w e)
  (q-join (q-se (q-ne w))
          (q-sw (q-nw e))
          (q-ne (q-se e))
          (q-ne (q-sw e))))

(defun centered-vertical (n s)
  (q-join (q-se (q-sw n))
          (q-sw (q-se n))
          (q-ne (q-nw s))
          (q-nw (q-ne s))))

(defun centered-sub-sub-node (node)
  (with-slots (nw ne sw se) node
    (q-join (q-se (q-se nw))
            (q-sw (q-sw ne))
            (q-ne (q-ne sw))
            (q-nw (q-nw se)))))


(defun next-generation (node)
  (with-slots (next-generation n level nw ne sw se) node
    (cond
      (next-generation
       next-generation)
      ((zerop n)
       (setf next-generation nw))

      ((= level 2)
       (setf next-generation (life-4x4 node)))

      (t
       (let ((n00 (next-generation nw))
             (n01 (horizontal-forward nw ne))
             (n02 (next-generation ne))
             (n10 (vertical-forward nw sw))
             (n11 (center-forward node))
             (n12 (vertical-forward ne se))
             (n20 (next-generation se))
             (n21 (horizontal-forward sw se))
             (n22 (next-generation se)))
         (setf next-generation
               (q-join
                (next-generation (q-join n00 n01 n10 n11))
                (next-generation (q-join n01 n02 n11 n12))
                (next-generation (q-join n10 n11 n20 n21))
                (next-generation (q-join n11 n12 n21 n22)))))))
    next-generation))

;; (defun next-generation (node)
;;   (multiple-value-bind (val found) (mm-get *next-gen-memo*  node)
;;     (cond
;;       (found
;;        val)
;;       ((not found)
;;        (let ((next-value (cond
;;                            ((zerop (q-n node))
;;                             (q-nw node))

;;                            ((= 2 (q-level node))

;;                             (life-4x4 node))

;;                            (t
;;                             (with-slots (nw ne sw se) node
;;                               (let ((n00 (centered-sub-node nw))
;;                                     (n01 (centered-horizontal nw ne))
;;                                     (n02 (centered-sub-node ne))

;;                                     (n10 (centered-vertical nw sw))
;;                                     (n11 (centered-sub-sub-node node))
;;                                     (n12 (centered-vertical ne se))

;;                                     (n20 (centered-sub-node sw))
;;                                     (n21 (centered-horizontal sw se))
;;                                     (n22 (centered-sub-node se)))
;;                                 (q-join (next-generation (q-join n00 n01 n11 n11))
;;                                         (next-generation (q-join n01 n02 n11 n12))
;;                                         (next-generation (q-join n10 n11 n20 n21))
;;                                         (next-generation (q-join n11 n12 n21 n22)))))))))
;;          (mm-add *next-gen-memo*
;;                  (list node)
;;                  next-value))))))

(defun inner-successors (m j)
  (let (
        ;; Top Row
        (c1 (successor (q-nw m)
                       j))

        (c2 (successor (q-join (q-ne (q-nw m))  (q-nw (q-ne m))

                               (q-se (q-nw m))  (q-sw (q-ne m)))
                       j))
        (c3 (successor (q-ne m)
                       j))

        ;; Middle Row
        (c4 (successor (q-join (q-sw (q-nw m))  (q-se (q-nw m))
                               (q-nw (q-sw m))  (q-ne (q-sw m)))
                       j))
        (c5 (successor (q-join (q-se (q-nw m))  (q-sw (q-ne m))
                               (q-ne (q-sw m))  (q-nw (q-se m)))
                       j))
        (c6 (successor (q-join (q-sw (q-ne m))  (q-se (q-ne m))
                               (q-nw (q-se m))  (q-ne (q-se m)))
                       j))

        ;; Bottom row
        (c7 (successor (q-sw m)
                       j))
        (c8 (successor (q-join (q-ne (q-sw m))  (q-nw (q-se m))
                               (q-se (q-sw m))  (q-sw (q-se m)))
                       j))
        (c9 (successor (q-se m)
                       j)))

    ;; (format t "m ~a~%" m)
    ;; (format t "(q-nw m) ~a~%(q-ne m) ~a~%(q-sw m) ~a~%(q-se m) ~a~%" (q-nw m) (q-ne m) (q-sw m) (q-se m) )
    ;; (format t "(q-nw (q-nw m)) ~a~%(q-ne (q-nw m)) ~a~%(q-sw (q-nw m)) ~a~%(q-se (q-nw m)) ~a~%" (q-nw (q-nw m)) (q-ne (q-nw m)) (q-sw (q-nw m)) (q-se (q-nw m)))
    ;; (format t "(q-nw (q-ne m)) ~a~%(q-ne (q-ne m)) ~a~%(q-sw (q-ne m)) ~a~%(q-se (q-ne m)) ~a~%" (q-nw (q-ne m)) (q-ne (q-ne m)) (q-sw (q-ne m)) (q-se (q-ne m)))
    ;; (format t "(q-nw (q-sw m)) ~a~%(q-ne (q-sw m)) ~a~%(q-sw (q-sw m)) ~a~%(q-se (q-sw m)) ~a~%" (q-nw (q-sw m)) (q-ne (q-sw m)) (q-sw (q-sw m)) (q-se (q-sw m)))
    ;; (format t "(q-nw (q-se m)) ~a~%(q-ne (q-se m)) ~a~%(q-sw (q-se m)) ~a~%(q-se (q-se m)) ~a~%" (q-nw (q-se m)) (q-ne (q-se m)) (q-sw (q-se m)) (q-se (q-se m)))
    ;; (format t "c1 ~a~%c2 ~a~%c3 ~a~%c4 ~a~%c5 ~a~%c6 ~a~%c7 ~a~%c8 ~a~%c9 ~a~%"
    ;;         c1 c2 c3 c4 c5 c6 c7 c8 c9)


    (values c1 c2 c3
            c4 c5 c6
            c7 c8 c9)))

(defun successor (node &optional (in-j (- (q-level node) 2)))
  "Return the 2**k-1 x 2**k-1 successor, 2**j generations in the future,
where j<= k-2, caching the result."
  (declare (type qtnode node)
           (type (or null integer)  in-j))
  (let* ((j (min in-j (- (q-level node) 2))))
    (when (< j 0)
      (error "j ~a < 0" j))
    (flet
        ((inner-successor ()
           (cond
             ((zerop (q-n node))
              (q-nw node))

             ((= 2 (q-level node))
              (life-4x4 node))

             (t
              (multiple-value-bind (c1 c2 c3
                                    c4 c5 c6
                                    c7 c8 c9)
                  (inner-successors node j)
                ;;(break)
                (if (< j (- (q-level node) 2))
                    (q-join
                     (q-join (q-se c1) (q-sw c2)
                             (q-ne c4) (q-nw c5))
                     (q-join (q-se c2) (q-sw c3)
                             (q-ne c5) (q-nw c6))

                     (q-join (q-se c4) (q-sw c5)
                             (q-ne c7) (q-nw c8))
                     (q-join (q-se c5) (q-sw c6)
                             (q-ne c8) (q-nw c9)))
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
                   :finally (return (2d-pt min-x min-y *on*))))
         (pattern (alexandria:shuffle (mapcar
                                       (lambda (pt)
                                         (2d-pt (- (pt-x pt) (pt-x min-pt))
                                             (- (pt-y pt) (pt-y min-pt))
                                             *on*))
                                       pts))))
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

             :collecting (2d-pt (ash (pt-x ept) -1)
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
  (q-join
   (q-se (q-nw node))
   (q-sw (q-ne node))
   (q-ne (q-sw node))
   (q-nw (q-se node))))

(defun crop (node)
  (cond ((or (<= (q-level node) 3 )
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
      :then (+ gens (ash 1 (- (q-level padded-node) 2)))
    :for i :below n
    :finally (return (values next-node gens))))

(defun advance (node n)
  "Advance node by exactly n generations, using the binary
expansion of n to find the correct successors."
  (declare (ignorable node n))
  (when (= 0 n)
    (return-from advance node))
  (let* ((bit-count (1+ (ceiling (log n 2))))
         (new-node (center node bit-count)))
    (when (null new-node)
      (format t "Warning: new-node is null!~%"))

    ;; (loop :for count :below n
    ;;       :for nod = (successor new-node )
    ;;         :then (successor nod 1)
    ;;       :finally (return nod))

    (loop
      :for k :from 0 :to bit-count
      :for bit = (logbitp k n)
      :for j = (- bit-count k 1)
      :for next-node = (if bit
                           (successor new-node j)
                           new-node)
        :then
        (if bit
            (successor next-node j)
            next-node)
      :finally
         (return (crop next-node)))))

(defun is-padded (node)
  (with-slots (nw ne sw se) node
    (and ;; (q-nw node)
     (= (q-n nw)
        (q-n (q-se (q-se nw))))

     (= (q-n ne)
        (q-n (q-sw (q-sw ne))))

     (= (q-n sw)
        (q-n (q-ne (q-ne sw))))

     (= (q-n se)
        (q-n (q-nw (q-nw se)))))))

(defun pad (node)
  (if (or (<= (q-level node) 3)
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

  (let* ((size (expt 2 (q-level node)))
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
      ((= (q-level node) level)

       ;; Scale quadtree to match scale of level 0
       (let ((reduction (- level)))
         (list
          (2d-pt (ash x reduction)
              (ash y reduction)
              (/ (q-n node)
                 (* size size))))))

      ;; Otherwise try expanding the children
      (t
       (with-slots (nw ne sw se) node
         (concatenate
          'list
          (expand nw
                  :x x
                  :y y
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)
          (expand ne
                  :x (+ x offset)
                  :y y
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)
          (expand sw
                  :x x
                  :y (+ y offset)
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)
          (expand se
                  :x (+ x offset)
                  :y (+ y offset)
                  :min-x min-x :max-x max-x
                  :min-y min-y :max-y max-y
                  :level level)))))))


(defun for-each-cell (node level function x y)
  "Turn a quadtree into a list of (x, y, gray) triples in
the rectangle (x,y) -> (lower-bound - upper-bound)"
  ;; Null or no cells on means skip over this whole quadtree
  (when (or (null node)
            (= 0 (q-n node)))
    (return-from for-each-cell nil))

  (let* ((size (expt 2 (q-level node)))
         (offset (ash size -1)))
    (cond
      ;; Collect cells at this zoom level
      ((= (q-level node) level)

       ;; Scale quadtree to match scale of level 0
       (funcall function
                (ash x (- level))
                (ash y (- level))
                (/ (q-n node)
                   (* size size))))

      ;; Otherwise try expanding the children
      (t
       (for-each-cell (q-nw node) level function x y)
       (for-each-cell (q-ne node) level function (+ x offset) y)
       (for-each-cell (q-sw node) level function x (+ y offset))
       (for-each-cell (q-se node) level function (+ x offset) (+ y offset))))))

(defun find-bounds (node level)
  "Turn a quadtree into a list of (x, y, gray) triples in
the rectangle (x,y) -> (lower-bound - upper-bound)"
  ;; Null or no cells on means skip over this whole quadtree
  (let ((min-x most-positive-fixnum)
        (max-x most-negative-fixnum)
        (min-y most-positive-fixnum)
        (max-y most-negative-fixnum))
    (flet ((update-stats (x y g)
             (declare (ignorable g))
             (setf min-x (min min-x x))
             (setf min-y (min min-y y))
             (setf max-x (max max-x x))
             (setf max-y (max max-y y))))
      (for-each-cell node level
                     #'update-stats
                     0 0)
      (values min-x min-y max-x max-y))))

(defun align (pts)
  (let* ((min-pt (loop
                   :for pt :in pts
                   :minimizing (pt-x pt) :into min-x
                   :minimizing (pt-y pt) :into min-y
                   :finally (return (2d-pt min-x min-y *on*)))))
    (sort (mapcar
           (lambda (pt)
             (2d-pt (- (pt-x pt) (pt-x min-pt))
                     (- (pt-y pt) (pt-y min-pt))
                     1))
           pts)
          #'pt-<)))

(defun same-pattern (pts expanded)
  (every #'hl::pt-= (align pts) (align expanded)))

(defun show-side-by-side (pat-name n &optional (stream t))
  (declare (type integer n))
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
