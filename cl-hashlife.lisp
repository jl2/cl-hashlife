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
         (type fixnum *mask*)
         (inline center q-join life life-4x4 read-life-file show-life-game)
         (optimize (speed 1) (space 3) (safety 3) (debug 3) (compilation-speed 0)))


(deftype maybe-node () '(or null qtnode))

(defstruct (qtnode (:conc-name q-) )
  (k 0 :type fixnum )
  (a nil :type maybe-node)
  (b nil :type maybe-node)
  (c nil :type maybe-node)
  (d nil :type maybe-node)
  (n nil :type fixnum)
  (hash nil :type integer))

(defparameter *on* (make-qtnode :k 0 :n 1 :hash 1)
  "Base level binary node 1")

(defparameter *off* (make-qtnode :k 0 :n 0 :hash 0)
  "Base level binary node 0")

(defparameter *mask* (1- (ash 1 62)))

(defun q-join (a b c d)
  "Combine four children at level `k-1` to a new node at level `k`.
If this is cached, return the cached node.
Otherwise, create a new node, and add it to the cache."
  (declare (type qtnode a b c d))
  (let ((n (loop :for var :in (list a b c d)
                 :summing (q-n var) :into sum fixnum
                 :finally (return sum)))
        (nhash (logand *mask*
                       (+ (q-k a)
                          (* 5131830419411
                             (q-hash a))
                          (* 3758991985019
                             (q-hash b))
                          (* 8973110871315
                             (q-hash c))
                          (* 4318490180473
                             (q-hash d))))))
    (declare (type fixnum n nhash))
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
  (declare (type fixnum k))
  (if (> k 0)
      (let ((omk (1- k)))
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
    (q-join
     (q-join z z z (q-a m))
     (q-join z z (q-b m) z)
     (q-join z (q-c m) z z)
     (q-join (q-d m) z z z))))

(defun life (a b c d e f g h i)
  "The standard life rule, taking eight neighbors and a center cell E.
Returns *on* if should be on, *off* otherwise."
  (declare (type qtnode a b c d e f g h i ))
  (let* ((all-nodes (list a b c d e f g h i))
         (outer (apply #'+ (mapcar #'q-n all-nodes))))
    (declare (type fixnum outer))
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
  (let ((ma (q-a m))
        (mb (q-b m))
        (mc (q-c m))
        (md (q-d m)))
    (q-join
     (life (q-a ma) (q-b ma) (q-a mb) (q-c ma) (q-d ma) (q-c mb) (q-a mc) (q-b mc) (q-a md))
     (life (q-b ma) (q-a mb) (q-b mb) (q-d ma) (q-c mb) (q-d mb) (q-b mc) (q-a md) (q-b md))
     (life (q-c ma) (q-d ma) (q-c mb) (q-a mc) (q-b mc) (q-a md) (q-c mc) (q-d mc) (q-c md))
     (life (q-d ma) (q-c mb) (q-d mb) (q-b mc) (q-a md) (q-b md) (q-d mc) (q-c md) (q-d md)))))

(defun successor (m &optional (j (- (q-k m) 2)))
  "Return the 2**k-1 x 2**k-1 successor, 2**j generations in the future,
where j<= k-2, caching the result."
  (declare (type qtnode m)
           (type fixnum j))
  (when (= (q-n m) 0)
    (return-from successor (q-a m)))
  (when (= (q-k m) 2)
    (return-from successor (life-4x4 m)))
  (let ((ma (q-a m))
        (mb (q-b m))
        (mc (q-c m))
        (md (q-d m))
        (j (min j
                (- (q-k m) 2))))
    (let ((c1 (successor (q-join (q-a ma) (q-b ma) (q-c ma) (q-d ma)) j))
          (c2 (successor (q-join (q-b ma) (q-a mb) (q-d ma) (q-c mb)) j))
          (c3 (successor (q-join (q-a mb) (q-b mb) (q-c mb) (q-d mb)) j))
          (c4 (successor (q-join (q-c ma) (q-d ma) (q-a mc) (q-b mc)) j))
          (c5 (successor (q-join (q-d ma) (q-c mb) (q-b mc) (q-a md)) j))
          (c6 (successor (q-join (q-c mb) (q-d mb) (q-a md) (q-b md)) j))
          (c7 (successor (q-join (q-a mc) (q-b mc) (q-c mc) (q-d mc)) j))
          (c8 (successor (q-join (q-b mc) (q-a md) (q-d mc) (q-c md)) j))
          (c9 (successor (q-join (q-a md) (q-b md) (q-c md) (q-d md)) j)))
      (if (< j (- (q-k m) 2))
          (q-join
           (q-join (q-d c1) (q-c c2) (q-b c4) (q-a c5))
           (q-join (q-d c2) (q-c c3) (q-b c5) (q-a c6))
           (q-join (q-d c4) (q-c c5) (q-b c7) (q-a c8))
           (q-join (q-d c5) (q-c c6) (q-b c8) (q-a c9))

           )
          (q-join
           (successor (q-join c1 c2 c4 c5) j)
           (successor (q-join c2 c3 c5 c6) j)
           (successor (q-join c4 c5 c7 c8) j)
           (successor (q-join c5 c6 c8 c9) j))))))

(fare-memoization:memoize 'successor)

(defun construct (pts)
  "Turn a list of x y coordinates into a quadtree and return the top level node."
  (multiple-value-bind (min-x min-y) (loop :for (x . y) :in pts
                                           :minimizing x :into min-x
                                           :minimizing y :into min-y
                                           :finally (return  (values min-x min-y)))
    (let ((pattern (alexandria:alist-hash-table
                    ;;(mapcar (lambda (wat) (cons wat *on*)) pts)
                    (loop :for (x . y) :in pts
                          :collecting (cons (cons (- x min-x)
                                                  (- y min-y))
                                            *on*))
                    :test 'equal)))
      (loop
        :for next-level = (make-hash-table :test 'equal) :then (make-hash-table :test 'equal)
        :while (/= (hash-table-count pattern) 1)
        :for k :from 0
        :for z = (get-zero k)
        :do
           (loop ;;:while (> (hash-table-count pattern) 0)
                 :for pt
                   :being :the hash-keys :of pattern
                 ;;:using (hash-value count)

                 ;;:for pt :in pattern
                 :for x = (car pt)
                 :for y = (cdr pt)
                 :for xn = (- x (logand x 1))
                 :for yn = (- y (logand y 1))
                 :for a = (gethash pt pattern z)
                 :for b = (gethash (cons (1+ x) y) pattern z)

                 :for c = (gethash (cons x (1+ y)) pattern z)

                 :for d = (gethash (cons (1+ x) (1+ y)) pattern z)
                 :do

                    (let ((nk (cons (ash x -1) (ash y -1)))
                          (joined  (q-join a b c d)))
                      (format t "x ~a~%y ~a~%xn ~a~%yn ~a~%a ~a~%b ~a~%c ~a~%d ~a~%nk ~a~%joined ~a~%"
                             x y xn yn a b c d nk joined)
                      (setf (gethash nk next-level) joined)))
           (format t "next-level: ~a ~%" next-level)
           (setf pattern next-level))
      (loop :for pt :being :the hash-values :of pattern
            :return pt))))
