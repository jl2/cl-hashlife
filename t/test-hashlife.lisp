;; hashlife.lisp
;;
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

(in-package :cl-user)
(defpackage :cl-hashlife.test.hashlife
  (:use :cl
        :fiveam
        :alexandria
        :cl-hashlife))

(in-package :cl-hashlife.test.hashlife)

(def-suite :cl-hashlife-hashlife)
(in-suite :cl-hashlife-hashlife)


(test bootstrap
    (loop
      :for i :below 2 :do
        (let* ((boot-2x2 (apply #'alexandria:map-product
                                #'hl::q-join
                                (loop
                                  :for i :below 4
                                  :collecting (list hl::*on* hl::*off*))))
               (boot-4x4 (apply #'alexandria:map-product
                                #'hl::q-join
                                (loop
                                  :for i :below 4
                                  :collecting boot-2x2)))
               (centers (loop
                          :for p :in boot-4x4
                          :collecting (cons p (hl::successor p 1)))))
          (is (= (+ 16 65536)
                 (hash-table-count hl::*join-table*)))
          (is (= 65536
                 (hash-table-count hl::*successor-table*)))
          )))

(test advance
      (let ((pat-node (hl::construct (test-pattern))))
        (loop :for i :below 32
              :for node = (hl::advance pat-node i)
              :do
                 (is (validate-tree node )))
        (loop :for i :below 4
              :do
                 (multiple-value-bind (node gens) (hl::ffwd pat-node i)
                   (is (validate-tree node))))
        (is (zerop (hl::q-n (hl::advance (hl::get-zero 8) 8))))
        (is (zerop (hl::q-n (hl::ffwd (hl::get-zero 8) 4))))))

(defun align (pts)
  (let* ((min-pt (loop
                   :for pt :in pts
                   :minimizing (hl::pt-x pt) :into min-x
                   :minimizing (hl::pt-y pt) :into min-y
                   :finally (return (hl::pt min-x min-y *on*)))))
    (sort (mapcar
           (lambda (pt)
             (hl::pt (- (hl::pt-x pt) (hl::pt-x min-pt))
                 (- (hl::pt-y pt) (hl::pt-y min-pt))
                 1))
           pts)
          #'hl::pt-<)))

(defun same-pattern (pts expanded)
  (every #'hl::pt-= (align pts) (align expanded)))

(defun verify-baseline (pat n)
  (let ((node (hl::construct pat)))
    (is (same-pattern pat (hl::expand node)))
    (loop
      :for i :below n
      :for pat = pat
        :then (hl::iterate-baseline-life pat)
      :for advanced = (hl::advance node i)
      :do (is (same-pattern pat (hl::expand advanced))))))

(test gray
  (let* ((pat (hl::read-game-file "breeder.lif"))
         (node (hl::construct pat))
         (total-on (length (hl::expand node))))
    (loop :for L :below 6
          :for expansion = (hl::expand node level=L)
          :for gray-sum = (reduce #'+ expansion :key #'cdr)
          :do (is (= gray-sum (/ total-on (expt 2 (* L 2))))))))

(defun validate-tree (node)
  (with-slots (a b c d n k) node
    (if (> k 0)
        (and (>= n 0)
             (<= n (expt 2 (* 2 k)))
             (= (hl:q-k a) (hl:q-k b) (hl:q-k c) (hl:q-k d))
             (= n (+ (hl:q-n a) (hl:q-n b) (hl:q-n c) (hl:q-n d)))
             (validate-tree a)
             (validate-tree b)
             (validate-tree c)
             (validate-tree d))
        t)))

(defun test-pattern ()
  (hl:read-game-file "GUN30.LIF"))

(test construct
  (is (validate-tree (hl:construct (test-pattern)))))

(test center
  (let ((test-pat (hl:construct (test-pattern))))
    (loop :for i :below 5
          :for old-node = test-pat :then node
          :for node = (hl::center test-pat) :then (hl::center node)
          :do
             (is (= (1+ (hl:q-k old-node)) (hl:q-k node)))
             (is (= (hl:q-n old-node) (hl:q-n node) ))
             (let ((ctr-node (hl::inner node)))
               (is (hl::q-hash ctr-node) (hl::q-hash old-node))))))

(test pad
  (let* ((test-pat (hl::construct (test-pattern)))
         (pad-pat (hl::pad test-pat))
         (crop-pat (hl::crop pad-pat)))
    
    (is (hl::is-padded pad-pat))
    (is (not (hl::is-padded crop-pat)))))
