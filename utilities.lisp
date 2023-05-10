;; baseline.lisp

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

(defparameter *game-file-dirs* (list
                                (asdf:system-relative-pathname :cl-hashlife "game-files/")
                                "~/data/life_games/"
                                "~/src/hashlife/lifep/"))


(deftype maybe-node () '(or null qtnode))

(defstruct (qtnode (:conc-name q-) )
  (k 1 :type integer)
  (a nil :type maybe-node)
  (b nil :type maybe-node)
  (c nil :type maybe-node)
  (d nil :type maybe-node)
  (n nil :type integer)
  (hash nil :type integer))

(defun get-address (node address)
  (loop
    :for next-node = node
      :then (slot-value next-node
                        (intern
                         (format nil "~c"
                                 (char-upcase addr))
                         :cl-hashlife))
    :for addr :across address
    :finally
       (return next-node)))

(defstruct (life-point (:conc-name pt-) )
  (x 0 :type integer)
  (y 0 :type integer)
  (gray 0 :type (or number qtnode)))

(defun pt (x y &optional (gray 1))
  (declare (type integer x y)
           (type (or number qtnode) gray))
  (make-life-point :x x
                :y y
                :gray gray))

(defun pt-< (a b)
  (declare (type life-point a b))
  (or (< (pt-y a) (pt-y b))
      (if (= (pt-y a) (pt-y b))
          (< (pt-x a) (pt-x b))
          nil)))

(defun pt-in-box (pt lower upper)
  (declare (type life-point pt lower upper))
  (and (<  (pt-x pt) (pt-x upper))
       (<  (pt-y pt) (pt-y upper))
       (> (pt-x pt) (pt-x lower))
       (> (pt-y pt) (pt-y lower))))

(defun pt-= (a b)
  (declare (type life-point a b))
  (and (= (pt-x a) (pt-x b))
       (= (pt-y a) (pt-y b))))

(defun pt-+ (a b)
  (with-slots ((x1 x) (y1 y) gray) a
    (with-slots ((x2 x) (y2 y)) b
      (make-life-point :x (+ x1 x2)
                       :y (+ y1 y2)
                       :gray gray))))

(defun pt-- (a b)
  (with-slots ((x1 x) (y1 y) gray) a
    (with-slots ((x2 x) (y2 y)) b
      (make-life-point :x (- x1 x2)
                       :y (- y1 y2)
                       :gray gray))))

(defun even-pt (pt)
  (with-slots (x y gray) pt
    (pt (- x (logand x 1))
        (- y (logand y 1))
        gray)))

(defun right-pt (pt)
  (with-slots (x y gray) pt
    (pt (1+ x)
        y
        gray)))

(defun down-pt (pt)
  (with-slots (x y gray) pt
    (pt x
        (1+ y)
        gray)))

(defun corner-pt (pt)
  (with-slots (x y gray) pt
    (pt (1+ x)
        (1+ y)
        gray)))


(defun read-rle-stream (stream)
  (let* ((header (loop ;; Skip beginning comment lines and find the x and y size line
                       :for line = (read-line stream nil nil)
                       :while (and line
                                   (eq #\# (aref line 0)))
                       :finally (return line)))
         ;; Split on , then =
         (sizes (mapcar (lambda (str)
                          (str:split #\= str))
                        (str:split #\, header)))
         ;; sizes will be something like:
         ;; (("x " " x-size" ) ("y " " y-size"))
         ;; cadar = (car (cdr (car list))) - second element of first list
         (x-size (read-from-string (cadar sizes)))
         ;; cadadr = (car (cdr (car (cdr list)))) = second element of second list
         (y-size (read-from-string (cadadr sizes)))
         (cur-x 0)
         (cur-y 0))
    (declare (ignorable y-size))
    (labels (
             ;; Advance by count cells, minding x-size and y-size
             (advance-by (count)
               (incf cur-y (floor (/ count x-size)))
               (incf cur-x (mod count x-size)))

             ;; Collect count live cells and advance by count, minding x-size and y-size
             (advance-live-cells (count)
               (loop :for i :below count
                     :collecting (pt cur-x cur-y)
                     :do (advance-by 1))))
      (loop
        :for line = (read-line stream nil nil)
        :while line
        :nconcing
        (loop
          :with cur-count = 0
          :for cur-char
            :across line

          :when (digit-char-p cur-char)
            ;; Increment the count
            :do (setf cur-count (+ (* 10 cur-count)
                                   (digit-char-p cur-char)))

          :when (char= #\$ cur-char)
            ;; $ - cur-count new lines
            :do (incf cur-y (max 1 cur-count))
                (setf cur-x 0
                      cur-count 0)

          :when (char= #\b cur-char)
            ;; b - cur-count dead cells
            :do (advance-by (max 1 cur-count))
                (setf cur-count 0)

          :when (char= #\o cur-char)
            ;; o - cur-count live cells
            :nconcing (let ((old-count cur-count))
                        (setf cur-count 0)
                        (advance-live-cells (max 1 old-count))))))))

(defun write-rle-stream (stream pts comment)
  (declare (ignorable stream pts comment))
  (format stream "# ~a~%" comment)
  ;; (multiple-value-bind (min-x min-y max-x max-y) (loop :for (x . y) :in pts
  ;;                                                      :minimizing x :into min-x
  ;;                                                      :minimizing y :into min-y
  ;;                                                      :maximizing x :into max-x
  ;;                                                      :maximizing y :into max-y
  ;;                                                      :finally (return (values min-x min-y max-x max-y)))
  ;;   (let ((x-size (- max-x min-x))
  ;;         (y-size (- max-y min-y))
  ;;         (cur-line-len 0)
  ;;         (max-line-len 80)
  ;;         (cur-live-count 0)
  ;;         )
  ;;     (declare (ignorable cur-line-len max-line-len cur-live-count))
  ;;     (flet ((process-live-cell (pt)
  ;;              )))
  ;;     ;; B3/S23 = "normal" Conways' Game of Life
  ;;     (format stream "x = ~a, y = ~a, rule = B3/S23~%" x-size y-size)
  ;;     (loop
  ;;       :with cur-line-len = 0
  ;;       :with max-line-len = 80
  ;;       :with cur-count = 0
  ;;       :with live-count = 0
  ;;       :for prev-x = min-x :then cur-x
  ;;       :for prev-y = min-y :then cur-y

  ;;       :for pt :in pts
  ;;       :for cur-x = (car pt)
  ;;       :for cur-y = (car pt)
  ;;       :for cur-run-x = (- cur-x prev-x)
  ;;       :for cur-run-y = (- cur-y prev-y)
  ;;       :when (and (= cur-run-x 1)
  ;;                  (= cur-run-y 0))
  ;;         :do (incf live-count 1)
  ;;       :when (= cur-run-y 1)
  ;;         :do
  ;;            (cond  ((> live-count 0)
  ;;                    (let ((new-data (format nil
  ;;                                            "~a~a"
  ;;                                             (if (= 1 live-count)
  ;;                                                 ""
  ;;                                                 live-count)
  ;;                                             "o")))
  ;;                      (when (> (+ (length new-data)
  ;;                                  cur-line-len)
  ;;                               max-line-len)
  ;;                        (format stream "~%")
  ;;                        (setf cur-line-len 0))
  ;;                      (format stream "~a$" new-data)
  ;;                      (incf cur-line-len (length new-data))
  ;;                      (setf live-count 0)
  ;;                      (setf cur-x min-x)))
  ;;                   (t
  ;;                    (format stream "$")))
  ;;       :when (or (> cur-run-y 0)
  ;;                 (> cur-run-x 1))
  ;;         :do
  ;;            (when (> live-count 0)
  ;;              (let ((new-data (format nil
  ;;                                      "~a~a"
  ;;                                      (if (= 1 live-count)
  ;;                                          ""
  ;;                                          live-count)
  ;;                                      "o")))
  ;;                (when (> (+ (length new-data)
  ;;                            cur-line-len)
  ;;                         max-line-len)
  ;;                  (format stream "~%")
  ;;                  (setf cur-line-len 0))
  ;;                (format stream "~a" new-data)
  ;;                (incf cur-line-len (length new-data))
  ;;                (setf live-count 0)))
  ;;            (let* ((dead-count (+ cur-run-x (* x-size cur-run-y )))
  ;;                   (new-data (format nil
  ;;                                     "~a~a"
  ;;                                     (if (= 1 dead-count)
  ;;                                         ""
  ;;                                         dead-count)
  ;;                                     "b")))
  ;;              (when (> (+ (length new-data)
  ;;                          cur-line-len)
  ;;                       max-line-len)
  ;;                (format stream "~%")
  ;;                (setf cur-line-len 0))
  ;;              (format stream "~a" new-data)
;;  (incf cur-line-len (length new-data))
  )

        ;; :for state-change = t :then (or (/= prev-y cur-y)
        ;;                                 (/= (1+ prev-x) cur-x))
        ;; :when (not state-change)
        ;;   :do (incf cur-count)
        ;; :when state-change
        ;;   :do (let ((new-update (format nil "~a~a" cur-count "o")))
        ;;         (cond  ((> (+ cur-line-len (length new-update)) max-line-len)
        ;;                 (format stream "~%~a" new-update)
        ;;                 (setf cur-line-len 0))
        ;;                (t
        ;;                 (format stream "~a" new-update)))
        ;;         (incf cur-line-len (length new-update)))))))
(defun read-life-1.06-stream (stream)
  (loop
      :with header = (read-line stream nil nil)
      :for x = (read stream nil nil)
      :for y = (read stream nil nil)
      :while (and x y)
      :collect (pt x y) :into pts
      :minimizing x :into min-x
      :minimizing y :into min-y
      :maximizing x :into max-x
      :maximizing y :into max-y
      :finally
         (return
           (let* ((rval (stable-sort pts #'pt-<)))
             (values rval
                     min-x min-y
                     max-x max-y)))))

(defun write-life-1.06-stream (stream pts comment)
  (format stream "# ~a~%" comment)
  (loop :for pt :in (sort (copy-list pts) #'pt-<) :do
    (format stream "~a ~a~%" (pt-x pt) (pt-y pt)))
  pts)

(defun read-life-1.05-stream (stream)
  (loop
    :for line-num :from 0
    :with off-x = 0
    :with off-y = 0
    :with this-y = 0
    :with header = (read-line stream nil nil)
    :for line = (read-line stream nil nil)
    :while line
    :when (and (> (length line) 2)
               (char= (aref line 0) #\#)
               (char= (aref line 1) #\P))
      :do
         (multiple-value-bind (val offset)
             (read-from-string line nil nil :start 3)
           (setf off-x val)
           (setf off-y (read-from-string line nil nil :start offset))
           (setf this-y off-y))
    :when (and (> (length line) 1)
               (or
                (char= (aref line 0) #\.)
                (char= (aref line 0) #\*)))
      :nconc
      (prog1
          (loop
            :for cell :across line
            :for this-x :from off-x
            :when (char= cell #\*)
              :collect (pt this-x this-y))
        (incf this-y))
    ))

(defun read-cells-stream (stream)
  (loop :for line = (loop ;; Skip beginning comment lines
                        :for line = (read-line stream nil nil)
                        :while (and line
                                    (eq #\! (aref line 0)))
                        :finally (return line))
          :then (read-line stream nil nil)
        :while line
        :for y :from 0
        :nconcing
        (loop
          :for next-char :across line
          :for x :from 0
          :when (or (eq next-char #\*) (eq next-char #\O))
            :collect (pt x y))))

(defun write-cells-stream (stream pts &optional (comment ""))
  (format stream "! ~a~%" comment)
  (multiple-value-bind (min-x min-y max-x max-y) (loop :for pt :in pts
                                                       :minimizing (pt-x pt) :into min-x
                                                       :maximizing (pt-x pt) :into max-x

                                                       :minimizing (pt-y pt) :into min-y
                                                       :maximizing (pt-y pt) :into max-y
                                                       :finally (return (values min-x min-y max-x max-y)))
    (let ((pts (sort (copy-list pts) #'pt-<)))
      (loop
        :for y :from min-y :to max-y :do
          (loop :for x :from min-x :to max-x
                :for elem = (pt x y)
                :do
                   (cond
                     ((find elem pts :test #'pt-=)
                      (setf pts (delete elem pts :test #'pt-=))
                      (format stream "O"))
                     (t (format stream "."))))
          (format stream "~%"))))
  pts)

(defun find-game-file (fname)
  (ctypecase fname
    (string
     (if (probe-file fname)
         fname
         (loop
           :for path :in *game-file-dirs*
           :do
              (when-let (the-file (probe-file (merge-pathnames fname path)))
                (return-from find-game-file (merge-pathnames fname path)))
           :finally (error "Could not find ~a" fname))))
    (pathname
     (probe-file fname))))


(defun find-reader (path)
  (let ((readers `(("life" . ,#'read-life-1.06-stream)
                   ("lif" . ,#'read-life-1.05-stream)
                   ("cells" . ,#'read-cells-stream)
                   ("rle" . ,#'read-rle-stream))))
    (assoc-value readers
                 (string-downcase (pathname-type path))
                 :test #'string= )))
(defun make-life (name-or-path)
  (read-game-file name-or-path))

(defun make-hashlife (name-or-path)
  (hl:construct (read-game-file name-or-path)))

(defun read-game-file (file-name-or-path)
  (let ((path (find-game-file file-name-or-path)))
    (with-open-file (stream path)
      (funcall (find-reader path) stream))))


(defun write-game-file (pts file-name &optional (comment "Written by cl-hashlife"))
  (cond
    ((str:ends-with? ".life" file-name)
     (with-open-file (stream (merge-pathnames (car *game-file-dirs*) file-name)
                             :direction :output
                             :if-exists :supersede
                             :external-format :utf8)
       (write-life-1.06-stream stream pts comment)))
    ((str:ends-with? ".cells" file-name)
     (with-open-file (stream (merge-pathnames (car *game-file-dirs*) file-name)
                             :direction :output
                             :if-exists :supersede
                             :external-format :utf8)
       (write-cells-stream stream pts comment)))))

(defgeneric show-life (node &optional stream level)
  (:documentation "Pretty print a life game to stream."))

(defmethod show-life ((node qtnode)
                      &optional (stream t) (level 0))
  (values node
          (show-life-list stream (expand node :level level) level)))

(defmethod show-life ((node string)
                      &optional (stream t) (level 0))
  (values node
          (show-life-list stream (read-game-file node) level)))

(defmethod show-life ((node list)
                      &optional (stream t) (level 0))
  (values node
          (show-life-list stream node level)))

(defun show-life-list (stream pts
                       &optional (level 0))
  (flet ((normalize (x)
           (ash x level)))
    (let ((cnt 0)
          (remaining-pts (sort pts
                               #'pt-<)))
      (multiple-value-bind (min-x min-y max-x max-y)
          (loop :for pt :in remaining-pts
                :minimizing (pt-x pt) :into min-x
                :minimizing (pt-y pt) :into min-y
                :maximizing (pt-x pt) :into max-x
                :maximizing (pt-y pt) :into max-y
                :finally (return (values min-x min-y max-x max-y)))
        (loop
          :for y :from (normalize min-y) :to (normalize max-y)
          :for real-y :from min-y :to max-y
          :do
            (loop
              :for x :from (normalize min-x) :to (normalize max-x)
              :for real-x :from min-x :to max-x
              :for elem = (pt real-x real-y)
              :do
                 (if-let ((it (find elem remaining-pts :test #'pt-= )))
                   (progn
                     ;;(declare (type life-point it))
                     (setf remaining-pts (remove elem remaining-pts :test #'pt-=))
                     (incf cnt)
                     (with-slots (gray) it
                       (cond ((> gray 0.7)
                              (format stream "██"))
                             ((> gray 0.5)
                              (format stream "▒▒"))
                             ((> gray 0.2)
                              (format stream "░░")))))
                   (format stream "  ")))
            (format stream "~%")))
      (format stream ".~%")
      (values cnt))))

(defparameter *baseline-temp-table* (make-hash-table :test 'equalp :size 100)
  "Avoid excessive hash table allocation in baseline-life.")

(defun baseline (ipts times)
   (loop 
         :for pts = ipts :then 
                        (iterate-baseline-life pts)
         :for i :below times
         :finally (return pts)))

(defun iterate-baseline-life (pts)
  "The baseline implementation of the Game of Life.
Takes a list of (x, y) cells and returns a new set of cells in
the next generation."

  (let ((counter (make-hash-table :test 'equalp :size 100)))

    ;; Count neighbors
    (loop
      :for pt :in pts
      :do
         (loop
           :for a :in '(-1 0 1)
           :do
              (loop
                :for b :in '(-1 0 1)
                :do
                   (incf (gethash (pt (+ (pt-x pt) a)
                                      (+ (pt-y pt) b))
                                  counter
                                  0)))))
    (loop
      :for pt
        :being :the hash-keys :of counter
          :using (hash-value count)
      :when (or (= count 3)
                (and (= count 4)
                     (find pt pts :test #'pt-=)))
        :collecting pt)))


(defun baseline-advance (pts
                         times)
  (loop
    :for game = pts :then (iterate-baseline-life game)
    :for i :below times
    :finally (return game)))
