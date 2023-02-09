;; cl-hashlife.lisp

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

(declaim (notinline get-zero)
         (type integer *mask*)
         (inline center q-join life life-4x4 read-life-file show-life-game)
         (optimize (speed 1) (space 3) (safety 3) (debug 3) (compilation-speed 0)))


(deftype maybe-node () '(or null qtnode))

(defstruct (qtnode (:conc-name q-) )
  (k 0 :type integer )
  (a nil :type maybe-node)
  (b nil :type maybe-node)
  (c nil :type maybe-node)
  (d nil :type maybe-node)
  (n nil :type integer)
  (hash nil :type integer))

(defparameter *on* (make-qtnode :k 0 :n 1 :hash 1)
  "Base level binary node 1")

(defparameter *off* (make-qtnode :k 0 :n 0 :hash 0)
  "Base level binary node 0")

(defparameter *mask* (1- (ash 1 63)))

(defun q-join (a b c d)
  "Combine four children at level `k-1` to a new node at level `k`.
If this is cached, return the cached node.
Otherwise, create a new node, and add it to the cache."
  (declare (type qtnode a b c d))
  (let ((n (loop :for var :in (list a b c d)
                 :summing (q-n var)))
        (nhash (logand *mask*
                       (+ (q-k a)
                          2
                          (* 5131830419411 (q-hash a))
                          (* 3758991985019 (q-hash b))
                          (* 8973110871315 (q-hash c))
                          (* 4318490180473 (q-hash d))))))
    (declare (type integer n nhash))
    (make-qtnode :k (1+ (q-k a))
                 :a a
                 :b b
                 :c c
                 :d d
                 :n n
                 :hash nhash)))

(fare-memoization:memoize 'q-join)

(defun get-zero (k)
  "Return an empty node at level `k`."
  (declare (type integer k))
  (if (>  0 k)
      (let ((omk (- k 1)))
        (q-join (get-zero omk)
                (get-zero omk)
                (get-zero omk)
                (get-zero omk)))
      *off*))

(fare-memoization:memoize 'get-zero)

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

(defun life (a b c d e f g h i)
  "The standard life rule, taking eight neighbors and a center cell E.
Returns *on* if should be on, *off* otherwise."
  (declare (type qtnode a b c d e f g h i ))
  (let* ((all-nodes (list a b c d e f g h i))
         (outer (apply #'+ (mapcar #'q-n all-nodes))))
    (declare (type integer outer))
    (if (or (= outer 3)
            (and (q-n e)
                 (= outer 2)))
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
             (life        m.a.a  m.a.b  m.b.a  m.a.c  m.a.d  m.b.c    m.c.a    m.c.b    m.d.a)

             ;; nb = life(m.a.b, m.b.a, m.b.b, m.a.d, m.b.c, m.b.d, m.c.b, m.d.a, m.d.b)  # BC
             (life        m.a.b  m.b.a  m.b.b  m.a.d  m.b.c  m.b.d  m.c.b  m.d.a  m.d.b)

             ;; nc = life(m.a.c, m.a.d, m.b.c, m.c.a, m.c.b, m.d.a, m.c.c, m.c.d, m.d.c)  # CB
             (life        m.a.c  m.a.d  m.b.c  m.c.a  m.c.b  m.d.a  m.c.c  m.c.d  m.d.c)

             ;; nd = life(m.a.d, m.b.c, m.b.d, m.c.b, m.d.a, m.d.b, m.c.d, m.d.c, m.d.d)  # DA
             (life        m.a.d  m.b.c  m.b.d  m.c.b  m.d.a  m.d.b  m.c.d  m.d.c  m.d.d))))))))


(defun successor (m &optional (j (- (q-k m) 2)))
  "Return the 2**k-1 x 2**k-1 successor, 2**j generations in the future,
where j<= k-2, caching the result."
  (declare (type qtnode m)
           (type integer j))
  (cond
    ((= (q-n m) 0))
    (q-a m)
    ((= (q-k m) 2)
     (life-4x4 m))
    (t
     (with-slots ((ma a) (mb b) (mc c) (md d)) m
       (with-slots ((m.a.a a) (m.a.b b) (m.a.c c) (m.a.d d)) ma
         (with-slots ((m.b.a a) (m.b.b b) (m.b.c c) (m.b.d d)) mb
           (with-slots ((m.c.a a) (m.c.b b) (m.c.c c) (m.c.d d)) mc
             (with-slots ((m.d.a a) (m.d.b b) (m.d.c c) (m.d.d d)) md

               (let (
                     ;; c1 = successor(join(m.a.a, m.a.b, m.a.c, m.a.d), j)
                     (c1 (successor (q-join m.a.a  m.a.b  m.a.c  m.a.d) j))

                     ;; c2 = successor(join(m.a.b, m.b.a, m.a.d, m.b.c), j)
                     (c2 (successor (q-join m.a.b  m.b.a  m.a.d  m.b.c) j))

                     ;; c3 = successor(join(m.b.a, m.b.b, m.b.c, m.b.d), j)
                     (c3 (successor (q-join m.b.a  m.b.b  m.b.c  m.b.d) j))

                     ;; c4 = successor(join(m.a.c, m.a.d, m.c.a, m.c.b), j)
                     (c4 (successor (q-join m.a.c  m.a.d  m.c.a  m.c.b) j))

                     ;; c5 = successor(join(m.a.d, m.b.c, m.c.b, m.d.a), j)
                     (c5 (successor (q-join m.a.d  m.b.c  m.c.b  m.d.a) j))

                     ;; c6 = successor(join(m.b.c, m.b.d, m.d.a, m.d.b), j)
                     (c6 (successor (q-join m.b.c  m.b.d  m.d.a  m.d.b) j))

                     ;; c7 = successor(join(m.c.a, m.c.b, m.c.c, m.c.d), j)
                     (c7 (successor (q-join m.c.a  m.c.b  m.c.c  m.c.d) j))

                     ;; c8 = successor(join(m.c.b, m.d.a, m.c.d, m.d.c), j)
                     (c8 (successor (q-join m.c.b  m.d.a  m.c.d  m.d.c) j))

                     ;; c9 = successor(join(m.d.a, m.d.b, m.d.c, m.d.d), j)
                     (c9 (successor (q-join m.d.a  m.d.b  m.d.c  m.d.d) j)))
                 (if (< j (- (q-k m) 2))
                     (q-join
                      (q-join (q-d c1) (q-c c2) (q-b c4) (q-a c5))
                      (q-join (q-d c2) (q-c c3) (q-b c5) (q-a c6))
                      (q-join (q-d c4) (q-c c5) (q-b c7) (q-a c8))
                      (q-join (q-d c5) (q-c c6) (q-b c8) (q-a c9)))
                     (q-join
                      (successor (q-join c1 c2 c4 c5) j)
                      (successor (q-join c2 c3 c5 c6) j)
                      (successor (q-join c4 c5 c7 c8) j)
                      (successor (q-join c5 c6 c8 c9) j))))))))))))

(fare-memoization:memoize 'successor)

(defun construct (pts)
  "Turn a list of x y coordinates into a quadtree and return the top level node."
  (let* ((remapped-points
           (loop :for (x . y) :in pts
                 :minimizing x :into min-x
                 :minimizing y :into min-y
                 :finally (return  (map 'list
                                        (lambda (pt)
                                          (cons (cons (- (car pt) min-x)
                                                      (- (cdr pt) min-y))
                                                *on*))
                                        pts)))))
    (loop
      :for pattern = (alexandria:alist-hash-table
                      remapped-points
                      :test 'equal)
        :then next-level
      :for next-level = (make-hash-table :test 'equal) :then (make-hash-table :test 'equal)
      :while (/= (hash-table-count pattern) 1)
      :for k :from 0
      :for z = (get-zero k)
      :do
         (format t "pattern ~a = ~{~a~%~^~%~}~%" k (alexandria:hash-table-alist pattern))
         (loop
           :while (> (hash-table-count pattern) 0)

             :for pt = (car (alexandria:hash-table-keys pattern))
             :for x = (car pt)
             :for y = (cdr pt)

             :for xn = (- x (logand x 1))
             :for yn = (- y (logand y 1))

             :for pt2 = (cons (1+ x) y)
             :for pt3 = (cons x (1+ y))
             :for pt4 = (cons (1+ x) (1+ y))

             :for a = (gethash pt  pattern z)
             :for b = (gethash pt2 pattern z)
             :for c = (gethash pt3 pattern z)
             :for d = (gethash pt4 pattern z)
             :do
                ;; (setf pat-keys (remove
                ;;                 pt4
                ;;                 (remove
                ;;                  pt3
                ;;                  (remove pt2
                ;;                          (remove pt pat-keys :test #'equal)
                ;;                          :test #'equal)
                ;;                  :test #'equal)
                ;;                 :test #'equal))
                ;; (format t "pat-keys ~a~%" pat-keys)
                (remhash pt pattern)
                (remhash pt2 pattern)
                (remhash pt3 pattern)
                (remhash pt4 pattern)
                (let ((nk (cons (ash x -1) (ash y -1)))
                      (joined  (q-join a b c d)))
                  (setf (gethash nk next-level) joined)))
      :finally (return (pad  (loop :for pt :being :the hash-values :of pattern :return pt)))
        )))

(defun inner (node)
  (q-join (q-d (q-a node))
          (q-c (q-b node))
          (q-b (q-c node))
          (q-a (q-d node))))

(defun crop (node)
  (cond ((or (<= (q-k node) 3)
             (not (is-padded node)))
         (pad (center node)))
        (t
         node)))

(defun ffwd (node n)

  (loop :for temp-node = (pad node) :then next-node
        :for gens = 0
          :then (+ gens (ash 1 (- (q-k temp-node) 2)))
        :for i :below n

        :for next-node = (successor temp-node)
        :finally (return (values next-node gens))))

(defun advance (node n)
  )
(defun is-padded (node)
  (and (= (q-n (q-a node))
          (q-n (q-d (q-d (q-a node)))))

       (= (q-n (q-b node))
          (q-n (q-c (q-c (q-b node)))))

       (= (q-n (q-c node))
          (q-n (q-b (q-b (q-c node)))))

       (= (q-n (q-d node))
          (q-n (q-a (q-a (q-d node)))))))

(defun pad (node)
  (if (or (<= (q-k node) 3)
          (is-padded node))
      (pad (center node))
      node))

(defun expand (node &key
                      (x 0) (y 0)
                      min-x max-x
                      min-y max-y
                      (level 0))
  "Turn a quadtree into a list of (x, y, gray) triples in
the rectangle (x,y) -> (lower-bound - upper-bound)"

  (cond ((null node)
         nil)
        ((= 0 (q-n node))
         nil)
        ((and min-x
              min-y
              max-x
              max-y
              (let ((size (expt 2 (q-k node))))
                (or (< (+ x size) min-x)
                    (< (+ y size) min-y)
                    (> (+ x size) max-x)
                    (> (+ y size) max-y))))
         (format t "Clipping: ~a ~a~%" x y)
         nil)
        ((= (q-k node) level)
         (let ((size (expt 2 (q-k node))))
           (list (cons (cons x y)
                       (/ (q-n node)
                          (* size size))))))
        (t
         (let ((size (expt 2 (q-k node))))
           (let ((offset (ash size -1)))
             (with-slots (a b c d) node
               (concatenate 'list
                            (expand a
                                    :x x  :y y
                                    :min-x min-x :max-x max-x
                                    :min-y min-y :max-y max-y
                                    :level level)
                            (expand b
                                    :x (+ x offset)  :y y
                                    :min-x min-x :max-x max-x
                                    :min-y min-y :max-y max-y
                                    :level level)
                            (expand c
                                    :x x :y (+ y offset)
                                    :min-x min-x :max-x max-x
                                    :min-y min-y :max-y max-y
                                    :level level)
                            (expand d
                                    :x (+ x offset) :y (+ y offset)
                                    :min-x min-x :max-x max-x
                                    :min-y min-y :max-y max-y
                                    :level level))))))))
