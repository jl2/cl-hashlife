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

(defclass hashlife-viz-context ()
  ((output-directory :initarg :output-directory)
   (base-filename :initform "life" :initarg :base-filename)
   (output-type :initform "svg")
   (dot-stream :initform nil)
   (label-nodes :initform nil :initarg :label-nodes)

   (line-type :initform "ortho" :initarg :line-type)
   (ranksep :initform 2.0 :initarg :ranksep)
   (nodesep :initform 2.0 :initarg :nodesep)

   (open-image :initform t :initarg :open-image)

   (depth :initform 20
          :initarg :depth)
   (expand-level :initform 0
                 :initarg :expand-level)
   (level :initform 1
          :initarg :level)
   (lower-left :initform nil
               :initarg :lower-left)
   (upper-right :initform nil
                :initarg :upper-right)

   (view-min :initform (vec2 0 0)
             :initarg :view-min)
   (view-width :initform (vec2 4000 4000)
               :initarg :view-width)
   (stroke-width :initform 0.08
                 :initarg :stroke-width)
   (stroke-color :initform (vec4 0.0 0.0 0.0 0.6)
                 :initarg :stroke-color)
   (fill-color :initform (vec4 0.1 0.8 0.1 1.0)
               :initarg :fill-color)

   (created-node-files :initform nil)
   (created-files :initform nil)
   (dot-nodes :initform nil))
  (:documentation "Context about a Hashlife visualization."))

(defun make-viz-context (output-directory &key
                                            (base-filename "life")
                                            (depth 20)
                                            (label-nodes nil)
                                            (level 1)
                                            (expand-level 0)
                                            (stroke-width 0.08)
                                            (stroke-color (vec4 0.1 0.1 0.1 0.6))
                                            (view-min (vec2 0 0))
                                            (view-width (vec2 4000 4000))
                                            (lower-left nil)
                                            (upper-right nil)
                                            (ranksep 2.0)
                                            (nodesep 2.0)
                                            (line-type "spline"))
  (ensure-directories-exist output-directory)
  (make-instance 'hashlife-viz-context
                 :base-filename base-filename
                 :output-directory output-directory
                 :depth depth
                 :level level
                 :label-nodes label-nodes
                 :expand-level expand-level
                 :stroke-width stroke-width
                 :stroke-color stroke-color
                 :view-min view-min
                 :view-width view-width
                 :lower-left lower-left
                 :upper-right upper-right

                 :ranksep ranksep
                 :nodesep nodesep
                 :line-type line-type))

(defun build-filename (viz-context filename type)
  (with-slots (output-directory) viz-context
    (format nil "~a~a.~a" output-directory filename type)))

(defun build-imagename (viz-context filename)
  (with-slots (output-directory output-type) viz-context
    (format nil "~a~a.~a" output-directory filename output-type)))

(defun build-qt-imagename (viz-context node)
  (with-slots (output-directory output-type) viz-context
    (format nil
            "~a~a.~a"
            output-directory
            (q-hash node)
            output-type)))

(defun build-animation-imagename (viz-context i)
  (with-slots (base-filename output-directory output-type) viz-context
    (format nil "~a/~a~8,'0d.~a" output-directory base-filename i output-type)))

(defun maybe-dot-attribute (thing value)
  (if value
      (format nil "~a=~s " thing value)
      ""))

(defmacro maybe-start-dot-file ((viz-context name) &body body)
  `(unwind-protect
        (progn
          (let ((should-start-and-finish-dot nil))
            (with-slots (base-filename created-files output-type dot-stream) ,viz-context
              (setf should-start-and-finish-dot (null dot-stream))
              (when should-start-and-finish-dot
                (start-dot-file viz-context ,name)))
            (let ((res (progn
                         ,@body)))
              (when should-start-and-finish-dot
                (format t "Finishing dot file!~%")
                (finish-dot-file viz-context))
              res)))))

(defun dot-node (viz-context node &optional name)
  (qt-to-svg viz-context node)
  (with-slots (dot-stream dot-nodes) viz-context
    (format dot-stream
              "~a [~ashape=custom shapefile=~s ]~%"
              (q-hash node)
              (maybe-dot-attribute "label" name)
              (build-qt-imagename viz-context node))
    ;; (when (not (zerop (q-n node)) )
    ;;   ;;when (not (find (q-hash node) dot-nodes :test #'=))
    ;;   (format dot-stream
    ;;           "~a [~ashape=custom shapefile=~s ]~%"
    ;;           (q-hash node)
    ;;           (maybe-dot-attribute "label" name)
    ;;           (build-qt-imagename viz-context node)))))
    ))
(defun show-qt-transition (viz-context a b label)
  (with-slots (dot-stream) viz-context
    (dot-node viz-context a)
    (dot-node viz-context b)
    ;;when (and (not (zerop (q-n nw))) (not (zerop (q-n b))))
    (format dot-stream
            "~a -> ~a [~a]~%"
            (q-hash a)
            (q-hash b)
            (maybe-dot-attribute "label" label))))

(defun start-dot-file (viz-context name)
  (with-slots (base-filename dot-stream line-type ranksep nodesep) viz-context
    (setf dot-stream (open (build-filename viz-context base-filename "dot")
                           :direction :output
                           :if-exists :supersede))
    (format dot-stream
            "digraph ~s {~%" name)
    (format dot-stream
            "splines=~s~%" line-type)
    (format dot-stream
            "ranksep=~f~%" ranksep)
    (format dot-stream
            "nodesep=~f~%" nodesep)))

(defun finish-dot-file (viz-context)
  (with-slots (base-filename dot-stream output-type created-files) viz-context
    (let* ((dot-filename (build-filename viz-context base-filename "dot"))
           (imagename (build-imagename viz-context base-filename))
           (dot-cmd (format nil
                            "dot -T~a -o~a ~a"
                            output-type
                            imagename
                            dot-filename)))
      (format dot-stream  "~%}~%")
      (close dot-stream)
      (push dot-filename created-files)
      (uiop:run-program dot-cmd :force-shell t :output t :error-output t )
      (push imagename created-files))))

(defun start-subgraph (viz-context name bgcolor)
  (with-slots (dot-stream) viz-context
    (format dot-stream
            "subgraph ~s {~%" name)
    (format dot-stream  "style=filled;~%color=~a;~%" bgcolor)))

(defun end-subgraph (viz-context)
  (with-slots (dot-stream) viz-context
    (format dot-stream
            "}~%")))


(defun to-svg (viz-context filename pts-or-node)
  (with-slots (lower-left upper-right level expand-level stroke-width view-min view-width created-files) viz-context
    (let ((remaining-pts
            ;; Sort cells from top left to bottom right
            (sort
             (etypecase pts-or-node
               (list
                ;; If pts-or-node is a list of points then crop it
                (if (and lower-left upper-right)
                    (crop-points pts-or-node lower-left upper-right)
                    pts-or-node))
               (qtnode
                ;; If pts-or-node is a qtnode then convert to pts and crop
                (if (and lower-left upper-right)
                    (expand pts-or-node
                            :min-x (pt-x lower-left)
                            :min-y (pt-y lower-left)
                            :max-x (pt-x upper-right)
                            :max-y (pt-y upper-right)
                            :level level)
                    (expand pts-or-node
                            :level  expand-level))))
             #'pt-<)))
      (with-output-to-file (outf filename
                                 :if-exists :supersede)

        (push filename created-files)
        (svg:with-svg (outf
                       (vx view-width) (vy view-width)
                       :default-stroke-width stroke-width
                       :view-min view-min
                       :view-width view-width)
          (svg:default-style outf)
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
                   (cell-width (/ (vx view-width) max-cells))
                   (cell-height (/ (vy view-width) max-cells))
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
                                                        1.0))))))))))))
(defun qt-to-svg (viz-context node &optional name-override)
  (with-slots (stroke-width output-directory stroke-color
               view-width view-min depth created-files created-node-files)
      viz-context
    (when (not (find (q-hash node)
                     created-node-files))
      (let ((real-filename (if name-override
                               name-override (build-qt-imagename viz-context node))))
        (with-output-to-file (outf real-filename
                                   :if-exists :supersede)
          (push real-filename created-files)
          (push (q-hash node) created-node-files)
          (svg:with-svg (outf
                         (vx view-width)
                         (vy view-width)
                         :default-stroke-width stroke-width
                         :default-stroke-color stroke-color
                         :view-min view-min
                         :view-width view-width)
            (svg:default-style outf)
            (labels
                ((draw-node (node current-depth min-x max-x min-y max-y)
                   (with-slots (k n nw ne sw se hash) node
                     (let* ((node-width (- max-x min-x))
                            (node-height (- max-y min-y))
                            (mid-x (+ min-x
                                      (/ node-width
                                         2)))
                            (mid-y (+ min-y
                                      (/ node-height
                                         2)))
                            (size (expt 2 (q-level node)))
                            (gray (- 1 (/ n (* size size) 1.0)))
                            (fill-color (vec4  (* 0.2 gray) (* 0.9 gray) (* 0.1 gray) 1)))

                       (svg:rectangle outf
                                      (vec2 min-x min-y)
                                      (vec2 node-width node-height)
                                      :fill-color fill-color)
                       (when (and  (= 1 n)
                                   (= 0 k))
                         (svg:circle outf
                                     (vec2 mid-x mid-y)
                                     (/ node-width 2.5)
                                     :fill-color (vec4 0 0 0 1)))

                       ;; Draw recursively when it's worth doing so
                       (when (and (> (q-level node) 0)
                                  (< current-depth depth)
                                  (not (zerop (q-n node))))

                         ;; Recursively draw nodes
                         (draw-node nw (1+ current-depth) min-x mid-x min-y mid-y)
                         (draw-node ne (1+ current-depth) mid-x max-x min-y mid-y)
                         (draw-node sw (1+ current-depth) min-x mid-x mid-y max-y)
                         (draw-node se (1+ current-depth) mid-x max-x mid-y max-y))))))

              (draw-node node 0 0 (vx view-width) 0 (vy view-width)))))))))


(defun animate-life (viz-context
                     pts-or-node
                     n)
  (loop
    :for i :below n
    :for pts = pts-or-node
      :then (etypecase pts
              (list (baseline-advance pts-or-node i))
              (qtnode (advance pts-or-node i)))
    :do
       (to-svg viz-context
               (build-animation-imagename viz-context i)
               pts)))

(defun animate-hashlife-qt (viz-context
                            start-node
                            n)
  (loop
    :for i :below n
    :for node = start-node
      :then (advance node 1)
    :do
       (qt-to-svg viz-context
                  node
                  (build-animation-imagename viz-context i))))

(defun show-hashlife-successors (viz-context
                                 start-node
                                 n)
  (loop
    :for node = start-node
      :then (successor node i)
    :for i :from 0 :below n
    :do
       (qt-to-svg viz-context
                  node
                  (build-animation-imagename viz-context i))))


(defun show-inner-successors (viz-context node j)


  (maybe-start-dot-file (viz-context "inner successors")
      (with-slots ((m.nw nw) (m.ne ne) (m.sw sw) (m.se se)) node
        (with-slots ((m.nw.nw nw) (m.nw.ne ne) (m.nw.sw sw) (m.nw.se se)) m.nw
          (with-slots ((m.ne.nw nw) (m.ne.ne ne) (m.ne.sw sw) (m.ne.se se)) m.ne
            (with-slots ((m.sw.nw nw) (m.sw.ne ne) (m.sw.sw sw) (m.sw.se se)) m.sw
              (with-slots ((m.se.nw nw) (m.se.ne ne) (m.se.sw sw) (m.se.se se)) m.se
                (let* (
                       ;; Top Row
                       (j1 (show-join viz-context
                                      m.nw.nw m.nw.ne
                                      m.nw.sw m.nw.se))
                       (c1 (show-successor viz-context j1 j))


                       (j2 (show-join viz-context
                                      m.nw.ne  m.ne.nw
                                      m.nw.se  m.ne.sw))
                       (c2 (show-successor viz-context j2 j))

                       (j3 (show-join viz-context
                                      m.ne.nw m.ne.ne
                                      m.ne.sw m.ne.se))
                       (c3 (show-successor viz-context j3 j))

                       ;; Middle Row
                       (j4 (show-join viz-context
                                      m.nw.sw  m.nw.se
                                      m.sw.nw  m.sw.ne))
                       (c4 (show-successor viz-context j4 j))

                       (j5 (show-join viz-context
                                      m.nw.se  m.ne.sw
                                      m.sw.ne  m.se.nw))
                       (c5 (show-successor viz-context j5 j))


                       (j6 (show-join viz-context
                                      m.ne.sw  m.ne.se
                                      m.se.nw  m.se.ne))
                       (c6 (show-successor viz-context
                                           j6
                                           j))

                       ;; Bottom row
                       (j7 m.sw)
                       (c7 (show-successor viz-context j7 j))

                       (j8 (show-join viz-context
                                      m.sw.ne m.se.nw
                                      m.sw.se m.se.sw))
                       (c8 (show-successor viz-context j8 j))

                       (j9 m.se)
                       (c9 (show-successor viz-context j9 j))
                       (inner-successors (list c1 c2 c3 c4 c5 c6 c7 c8 c9))
                       (names '("c1" "c2" "c3" "c4" "c5" "c6" "c7" "c8" "c9")))

                  (start-subgraph viz-context "inner-successor" "lightgrey")
                  (qt-to-svg viz-context node)
                  (dot-node viz-context node "node")
                  (loop
                    :for inner-successor :in inner-successors
                    :for name :in names
                    :do
                       (qt-to-svg viz-context inner-successor)
                       (dot-node viz-context inner-successor name)
                       (show-qt-transition viz-context node inner-successor (format nil "inner successor ~a" name)))
                  (end-subgraph viz-context)
                  (values c1 c2 c3 c4 c5 c6 c7 c8 c9)))))))))

(defun show-life-viz (a b c
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

(defun show-life-4x4 (viz-context m)
  "Return the next generation of a k=2 (i.e. 4x4) cell.
To terminate the recursion at the base level,
if we have a k=2 4x4 block,
we can compute the 2x2 central successor by iterating over all
the 3x3 sub-neighborhoods of 1x1 cells using the standard life rule."
  (declare (type qtnode m))
  (maybe-start-dot-file (viz-context "life-4x4")
    (with-slots ((mnw nw) (mne ne) (msw sw) (mse se)) m
      (with-slots ((m.nw.nw nw) (m.nw.ne ne) (m.nw.sw sw) (m.nw.se se)) mnw
        (with-slots ((m.ne.nw nw) (m.ne.ne ne) (m.ne.sw sw) (m.ne.se se)) mne
          (with-slots ((m.sw.nw nw) (m.sw.ne ne) (m.sw.sw sw) (m.sw.se se)) msw
              (with-slots ((m.se.nw nw) (m.se.ne ne) (m.se.sw sw) (m.se.se se)) mse
                (let ((result
                        (show-join viz-context
                         ;; Copy/pasted from. .(find-file-other-window "~/src/hashlife/hashlife.py" )
                         ;; na = life(m.a.a, m.nw.ne, m.ne.nw, m.nw.sw, m.nw.se, m.ne.sw, m.sw.nw, m.sw.ne, m.se.nw)  # AD
                         (show-life-viz  m.nw.nw  m.nw.ne  m.ne.nw
                                         m.nw.sw  m.nw.se  m.ne.sw
                                         m.sw.nw  m.sw.ne  m.se.nw)

                         ;; nb = life(m.nw.ne, m.ne.nw, m.ne.ne, m.nw.se, m.ne.sw, m.ne.se, m.sw.ne, m.se.nw, m.se.ne)  # BC
                         (show-life-viz   m.nw.ne  m.ne.nw  m.ne.ne
                                          m.nw.se  m.ne.sw  m.ne.se
                                          m.sw.ne  m.se.nw  m.se.ne)

                         ;; nc = life(m.nw.sw, m.nw.se, m.ne.sw, m.sw.nw, m.sw.ne, m.se.nw, m.sw.sw, m.sw.se, m.se.sw)  # CB
                         (show-life-viz  m.nw.sw  m.nw.se  m.ne.sw
                                      m.sw.nw  m.sw.ne  m.se.nw
                                      m.sw.sw  m.sw.se  m.se.sw)

                         ;; nd = life(m.nw.se, m.ne.sw, m.ne.se, m.sw.ne, m.se.nw, m.se.ne, m.sw.se, m.se.sw, m.se.se)  # DA
                         (show-life-viz   m.nw.se  m.ne.sw  m.ne.se
                                      m.sw.ne  m.se.nw  m.se.ne
                                      m.sw.se  m.se.sw  m.se.se))))
                  result))))))))

(defun show-join (viz-context nw ne sw se)
  (multiple-value-bind (val found) (mm-get *join-memo* nw ne sw se)
    (cond
      (found
       val)
      ((not found)

       (mm-add *join-memo* (list nw ne sw se)
               (progn
                 (maybe-start-dot-file (viz-context "show-join")
                   (let* ((this-level (if (zerop (1+ (q-level nw)))
                                      (progn (break) 1)
                                      (1+ (q-level nw))))

                          (result (make-qtnode :level this-level
                                               :nw nw
                                               :ne ne
                                               :sw sw
                                               :se se
                                               :n (+ (q-n nw)
                                                     (q-n ne)
                                                     (q-n sw)
                                                     (q-n se))
                                               :hash (compute-hash nw ne sw se))))
                     (qt-to-svg viz-context nw)
                     (qt-to-svg viz-context ne)
                     (qt-to-svg viz-context sw)
                     (qt-to-svg viz-context se)
                     (qt-to-svg viz-context result)
                     (dot-node viz-context nw "nw")
                     (dot-node viz-context ne "ne")
                     (dot-node viz-context sw "sw")
                     (dot-node viz-context se "se")
                     (dot-node viz-context result "joined")
                     (show-qt-transition viz-context nw result "nw")
                     (show-qt-transition viz-context ne result "ne")
                     (show-qt-transition viz-context sw result "sw")
                     (show-qt-transition viz-context se result "se")
                     result))))))))


(defun show-successor (viz-context node j)
  (declare (type qtnode node)
           (type (or null integer)  j))
  (maybe-start-dot-file (viz-context "successor")
    (qt-to-svg viz-context node)
    (dot-node viz-context node "node")
    (flet ((real-inner-show-successor ()
             (cond
               ((zerop (q-n node))
                (show-qt-transition viz-context node *off* "successor 0")
                (q-nw node))

               ((= (q-level node) 2)
                (let ((lf4 (show-life-4x4 viz-context node)))
                  (qt-to-svg viz-context lf4)
                  (show-qt-transition viz-context node lf4 "successor life-4x4")
                  lf4))

               (t
                (with-slots ((m.nw nw) (m.ne ne) (m.sw sw) (m.se se)) node
                  (with-slots ((m.nw.nw nw) (m.nw.ne ne) (m.nw.sw sw) (m.nw.se se)) m.nw
                    (with-slots ((m.ne.nw nw) (m.ne.ne ne) (m.ne.sw sw) (m.ne.se se)) m.ne
                      (with-slots ((m.sw.nw nw) (m.sw.ne ne) (m.sw.sw sw) (m.sw.se se)) m.sw
                        (with-slots ((m.se.nw nw) (m.se.ne ne) (m.se.sw sw) (m.se.se se)) m.se

                          (format t "ma ~a~%mb ~a~%mc ~a~%md ~a~%" m.nw m.ne m.sw m.se)
                          (format t "maa ~a~%mab ~a~%mac ~a~%mad ~a~%" m.nw.nw m.nw.ne m.nw.sw m.nw.se)
                          (format t "maa ~a~%mab ~a~%mac ~a~%mbd ~a~%" m.ne.nw m.ne.ne m.ne.sw m.ne.se)
                          (format t "maa ~a~%mab ~a~%mac ~a~%mcd ~a~%" m.sw.nw m.sw.ne m.sw.sw m.sw.se)
                          (format t "maa ~a~%mab ~a~%mac ~a~%mdd ~a~%" m.se.nw m.se.ne m.se.sw m.se.se)

                          (let* (
                                 ;; Top Row
                                 (j1 (show-join viz-context
                                                m.nw.nw m.nw.ne
                                                m.nw.sw m.nw.se))
                                 (c1 (show-successor viz-context j1 j))


                                 (j2 (show-join viz-context
                                                m.nw.ne  m.ne.nw
                                                m.nw.se  m.ne.sw))
                                 (c2 (show-successor viz-context j2 j))


                                 (j3 (show-join viz-context
                                                m.ne.nw m.ne.ne
                                                m.ne.sw m.ne.se))
                                 (c3 (show-successor viz-context j3 j))


                                 ;; Middle Row
                                 (j4 (show-join viz-context
                                                m.nw.sw  m.nw.se
                                                m.sw.nw  m.sw.ne))
                                 (c4 (show-successor viz-context j4 j))


                                 (j5 (show-join viz-context
                                                m.nw.se  m.ne.sw
                                                m.sw.ne  m.se.nw))
                                 (c5 (show-successor viz-context j5 j))


                                 (j6 (show-join viz-context
                                                m.ne.sw  m.ne.se
                                                m.se.nw  m.se.ne))
                                 (c6 (show-successor viz-context j6 j))

                                 ;; Bottom row

                                 (j7 m.sw)
                                 (c7 (show-successor viz-context j7 j))


                                 (j8 (show-join viz-context
                                                m.sw.ne m.se.nw
                                                m.sw.se m.se.sw))
                                 (c8 (show-successor viz-context j8 j))


                                 (j9 m.se)
                                 (c9 (show-successor viz-context j9 j)))
                            (flet ((i-join (nw ne sw se)
                                     (show-join viz-context nw ne sw se)))
                              (cond ((< j (- (q-level node) 2))
                                     (start-subgraph viz-context "inner-successors" "blue")
                                     (let* ((la (i-join (q-se c1) (q-sw c2)
                                                        (q-ne c4) (q-nw c5)))
                                            (lb (i-join (q-se c2) (q-sw c3)
                                                        (q-ne c5) (q-nw c6)))
                                            (lc (i-join (q-se c4) (q-sw c5)
                                                        (q-ne c7) (q-nw c8)))
                                            (ld (i-join (q-se c5) (q-sw c6)
                                                        (q-ne c8) (q-nw c9)))
                                            (joined (i-join la lb lc ld)))
                                       (end-subgraph viz-context)
                                       joined)
                                     )
                                    (t
                                     (start-subgraph viz-context "inner2" "red")
                                     (let* ((ta (i-join c1 c2
                                                        c4 c5))
                                            (tb (i-join c2 c3
                                                        c5 c6))
                                            (tc (i-join c4 c5
                                                        c7 c8))
                                            (td (i-join c5 c6
                                                        c8 c9))
                                            (sa (show-successor viz-context ta j))
                                            (sb (show-successor viz-context tb j))
                                            (sc (show-successor viz-context tc j))
                                            (sd (show-successor viz-context td j))
                                            (result (show-join viz-context sa sb sc sd)))

                                       (qt-to-svg viz-context ta)
                                       (qt-to-svg viz-context tb)
                                       (qt-to-svg viz-context tc)
                                       (qt-to-svg viz-context td)
                                       (qt-to-svg viz-context sa)
                                       (qt-to-svg viz-context sb)
                                       (qt-to-svg viz-context sc)
                                       (qt-to-svg viz-context sd)
                                       (show-qt-transition viz-context ta sa "successor a")
                                       (show-qt-transition viz-context tb sb "successor b")
                                       (show-qt-transition viz-context tc sc "successor c")
                                       (show-qt-transition viz-context td sd "successor d")
                                       (end-subgraph viz-context)
                                       (show-qt-transition viz-context node result (format nil "successor ~a" j))
                                       result))))))))))))))
      (multiple-value-bind (val found) (mm-get *successor-memo* node j)
        (cond
          (found
           (show-qt-transition viz-context node val (format nil "successor ~a" j))
           val)

          ((not found)
           (let ((it (real-inner-show-successor)))
             (mm-add *successor-memo*
                     (list node j)
                     it)
             (show-qt-transition viz-context node it (format nil "successor ~a" j))
             it)))))))

(defun show-advance (viz-context node n)
  "Advance node by exactly n generations, using the binary
expansion of n to find the correct successors."

  (declare (ignorable node n))

  (when (= 0 n)
    (return-from show-advance node))

  (maybe-start-dot-file (viz-context "advance")
    (let* ((bit-count (1+ (ceiling (log n 2))))
           (new-node (loop
                       :for new-node = (center node)
                         :then (center new-node)
                       :for k :below bit-count
                       :finally
                          (return new-node))))
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
        :for prev-node = new-node :then next-node
        :for next-node = (if bit
                             (successor new-node j)
                             new-node)
          :then
          (if bit
              (successor next-node j)
              next-node)
        :do (show-qt-transition viz-context
                                prev-node
                                next-node
                                (format nil "advance ~a" j))

        :finally (return next-node)))))
