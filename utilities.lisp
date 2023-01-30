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

(declaim (optimize (speed 1)
                   (space 0)
                   (safety 0)
                   (debug 0)
                   (compilation-speed 0)))

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
                     :collecting (cons cur-x cur-y)
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
  (format stream "# ~a~%" comment)
  (multiple-value-bind (min-x min-y max-x max-y) (loop :for (x . y) :in pts
                                                       :minimizing x :into min-x
                                                       :minimizing y :into min-y
                                                       :maximizing x :into max-x
                                                       :maximizing y :into max-y
                                                       :finally (return (values min-x min-y max-x max-y)))
    (let ((x-size (- max-x min-x))
          (y-size (- max-y min-y))
          (cur-line-len 0)
          (max-line-len 80)
          (cur-live-count 0)
          )
      (flet ((process-live-cell (pt)
               )))
      ;; B3/S23 = "normal" Conways' Game of Life
      (format stream "x = ~a, y = ~a, rule = B3/S23~%" x-size y-size)
      (loop
        :with cur-line-len = 0
        :with max-line-len = 80
        :with cur-count = 0
        :with live-count = 0
        :for prev-x = min-x :then cur-x
        :for prev-y = min-y :then cur-y
        
        :for pt :in pts
        :for cur-x = (car pt)
        :for cur-y = (car pt)
        :for cur-run-x = (- cur-x prev-x)
        :for cur-run-y = (- cur-y prev-y)
        :when (and (= cur-run-x 1)
                   (= cur-run-y 0))
          :do (incf live-count 1)
        :when (= cur-run-y 1)
          :do
             (cond  ((> live-count 0)
                     (let ((new-data (format nil
                                             "~a~a"
                                              (if (= 1 live-count)
                                                  ""
                                                  live-count)
                                              "o")))
                       (when (> (+ (length new-data)
                                   cur-line-len)
                                max-line-len)
                         (format stream "~%")
                         (setf cur-line-len 0))
                       (format stream "~a$" new-data)
                       (incf cur-line-len (length new-data))
                       (setf live-count 0)
                       (setf cur-x min-x)))
                    (t
                     (format stream "$")))
        :when (or (> cur-run-y 0)
                  (> cur-run-x 1))
          :do
             (when (> live-count 0)
               (let ((new-data (format nil
                                       "~a~a"
                                       (if (= 1 live-count)
                                           ""
                                           live-count)
                                       "o")))
                 (when (> (+ (length new-data)
                             cur-line-len)
                          max-line-len)
                   (format stream "~%")
                   (setf cur-line-len 0))
                 (format stream "~a" new-data)
                 (incf cur-line-len (length new-data))
                 (setf live-count 0)))
             (let* ((dead-count (+ cur-run-x (* x-size cur-run-y )))
                    (new-data (format nil
                                      "~a~a"
                                      (if (= 1 dead-count)
                                          ""
                                          dead-count)
                                      "b")))
               (when (> (+ (length new-data)
                           cur-line-len)
                        max-line-len)
                 (format stream "~%")
                 (setf cur-line-len 0))
               (format stream "~a" new-data)
               (incf cur-line-len (length new-data)))))))
              
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
(defun read-life-stream (stream)
  (loop
      :with header = (read-line stream nil nil)
      :for x = (read stream nil nil)
      :for y = (read stream nil nil)
      :while (and x y)
      :collect (cons x y) :into pts
      :minimizing x :into min-x
      :minimizing y :into min-y
      :maximizing x :into max-x
      :maximizing y :into max-y
      :finally
         (return
           (let* ((tmp (stable-sort pts
                                   #'<
                                   :key
                                   #'cdr))
                  (tmp2 (stable-sort tmp
                                     #'<
                                     :key #'car)))
             (multiple-value-bind (val offset) (read-from-string "3 4" ) (values val (subseq "3 4" offset)))
             (values tmp2
                     min-x min-y
                     max-x max-y)))))

(defun write-life-stream (stream pts comment)
  (format stream "# ~a~%" comment)
  (loop :for (x . y) :in pts :do
    (format stream "~a ~a~%" x y))
  pts)


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
            :collect (cons x y))))

(defun write-cells-stream (stream pts &optional (comment ""))
  (format stream "! ~a~%" comment)
  (multiple-value-bind (min-x min-y max-x max-y) (loop :for (x . y) :in pts
                                                       :minimizing x :into min-x
                                                       :minimizing y :into min-y
                                                       :maximizing x :into max-x
                                                       :maximizing y :into max-y
                                                       :finally (return (values min-x min-y max-x max-y)))
    (let ((pts pts))
      (loop
        :for y :from min-y :to max-y :do
          (loop :for x :from min-x :to max-x
                :for elem = (cons x y)
                :do
                   (cond
                     ((find elem pts :test #'equal)
                      (setf pts (delete elem pts))
                      (format stream "O"))
                     (t (format stream "."))))
          (format stream "~%"))))
  pts)

(defparameter *game-file-dirs* (list (asdf:system-relative-pathname :cl-hashlife "game-files/")
                                     "~/data/life_games/"))


(defun find-game-file (fname)
  (loop
    :for path :in *game-file-dirs*
    :until (probe-file (merge-pathnames fname path))
    :finally (return (merge-pathnames fname path))))


(defun read-game-file (file-name)
  (cond ((str:ends-with? ".life" file-name)
         (with-open-file (stream (find-game-file file-name))
           (read-life-stream stream)))
        ((str:ends-with? ".cells" file-name)
         (with-open-file (stream (find-game-file file-name))
           (read-cells-stream stream)))
        ((str:ends-with? ".rle" file-name)
         (with-open-file (stream (find-game-file file-name))
           (read-rle-stream stream)))))

(defun write-game-file (pts file-name &optional (comment "Written by cl-hashlife"))
  (cond ((str:ends-with? ".life" file-name)
         (with-open-file (stream (concatenate 'string *game-file-directory* file-name)
                                 :direction :output
                                 :if-exists :supersede
                                 :external-format :utf8)
           (write-life-stream stream pts comment)))
        ((str:ends-with? ".cells" file-name)
         (with-open-file (stream (concatenate 'string *game-file-directory* file-name)
                                 :direction :output
                                 :if-exists :supersede
                                 :external-format :utf8)
           (write-cells-stream stream pts comment)))))


(defun show-life-game (stream pts)
  (let ((cnt 0)
        (pts pts))
    (multiple-value-bind (min-x min-y max-x max-y) (loop :for (x . y) :in pts
                                                         :minimizing x :into min-x
                                                         :minimizing y :into min-y
                                                         :maximizing x :into max-x
                                                         :maximizing y :into max-y
                                                         :finally (return (values min-x min-y max-x max-y)))
      (loop
        :for y :from min-y :to max-y :do
          (loop :for x :from min-x :to max-x
                :for elem = (cons x y)
                :do
                   (cond
                     ((find elem pts :test #'equal)
                      (remove elem pts)
                      (incf cnt)
                      (format stream "██"))
                     (t (format stream "  "))))
          (format stream "~%")
        )
      cnt)))

(defparameter *baseline-temp-table* (make-hash-table :test 'equal :size 100)
  "Avoid excessive hash table allocation in baseline-life.")

(defun iterate-baseline-life (pts)
  "The baseline implementation of the Game of Life.
Takes a list of (x, y) cells and returns a new set of cells in
the next generation."

  (clrhash *baseline-temp-table*)
  (loop
    :for (x . y) :in pts
    :do
       (loop
         :for a :in '(-1 0 1)
         :do
            (loop
              :for b :in '(-1 0 1)
              :do
                 (incf (gethash (cons (+ x a)
                                      (+ y b))
                                *baseline-temp-table*
                                0)))))
  (loop
    :for pt
      :being :the hash-keys :of *baseline-temp-table*
        :using (hash-value count)
    :when (or (= count 3)
              (and (= count 4)
                   (find pt pts :test #'equal)))
      :collecting pt))


(defun iterate-game-of-life (file-name-or-game-data
                             times
                             &key
                               (iterator #'hl:iterate-baseline-life)
                               (printer (lambda (x)
                                          (show-life-game *standard-output* x)
                                          (format *standard-output*
                                                  "~a~%"
                                                  (make-string 120 :initial-element #\=)))))
  (loop
    :with initial-data = (etypecase file-name-or-game-data
                           (string
                            (read-game-file file-name-or-game-data))
                           (list file-name-or-game-data))
    :for game = initial-data :then (funcall iterator game)
    :for i :below (1+ times)
    :do
       (funcall printer game)
    :finally (return game)))
