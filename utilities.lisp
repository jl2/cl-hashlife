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


(defun make-life (name-or-path)
  "Create a life game from a file name or path.
Searches *game-file-dirs*, so full paths can be left off."
  (read-game-file name-or-path))

(defun make-hashlife (name-or-path)
  "Construct a Hashlife quadtree node from a file name or path.
Searches *game-file-dirs*, so full paths can be left off.
Like `make-life`, but constructs a Hashlife quadtree."
  (hl:construct (read-game-file name-or-path)))


(defgeneric show-life-iteration (node n &optional stream level)
  (:documentation "Pretty print a life game iteration to stream."))

(defmethod show-life-iteration ((node qtnode)
                                n
                                &optional (stream t) (level 0))
  (loop
    :for i :to n
    :for current = (advance node i)
    :do
       (show-life current stream level)
    :finally (return current)))

(defmethod show-life-iteration ((node string)
                                n
                                &optional (stream t) (level 0))
  (show-life-iteration (make-hashlife node) n stream level))

(defmethod show-life-iteration ((node list)
                                n
                                &optional (stream t) (level 0))
  (loop
    :for current = node
      :then (iterate-baseline-life current)
    :for i :to n
    :do
       (show-life current stream level)
    :finally (return current)))



(defgeneric show-life (node &optional stream level)
  (:documentation "Pretty print a life game to stream."))

(defmethod show-life ((node qtnode)
                      &optional (stream t) (level 0))
  (values node
          (show-life-list stream (expand node :level level) level)))

(defmethod show-life ((node string)
                      &optional (stream t) (level 0))
  (show-life (make-life node) stream level))

(defmethod show-life ((node list)
                      &optional (stream t) (level 0))
  (values node
          (show-life-list stream node level)))

(defun show-life-list (stream pts
                       &optional (level 0))
  "Write a life cell pattern to stream."
  (declare (type (or  stream t) stream)
           (type  list pts)
           (type  fixnum level))
  (flet ((normalize (x)
           (declare (type fixnum x))
           (the fixnum (ash x level))))

    (let ((cnt 0)
          (remaining-pts (sort pts
                               #'pt-<)))
      (declare (type fixnum cnt)
               (type list remaining-pts))
      (multiple-value-bind (min-x min-y max-x max-y) (game-bounds remaining-pts)
        (declare (type fixnum min-x min-y max-x max-y))
        (loop
          :for y fixnum :from (normalize min-y)
            :to (normalize max-y)
          :for real-y fixnum :from min-y
            :to max-y
          :do
             (loop
               :for x fixnum :from (normalize min-x) :to (normalize max-x)
               :for real-x fixnum :from min-x :to max-x
               :for elem = (2d-pt real-x real-y)
               :do
                  (if-let ((it (find elem remaining-pts :test #'pt-=)))
                    (progn
                      (setf remaining-pts (remove elem remaining-pts :test #'pt-=))
                      (incf cnt)
                      (with-slots (gray) it
                        (declare (type number gray))
                        (cond ((> gray 0.7)
                               (format stream "██"))
                              ((> gray 0.5)
                               (format stream "▒▒"))
                              ((> gray 0.2)
                               (format stream "░░")))))
                    (format stream "  ")))
             (format stream "~%")))
      (format stream "⛳~%")
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
  (declare (dynamic-extent pts))
  (let ((counter (make-hash-table :test 'equalp
                                  :size 100)))
    (declare (dynamic-extent counter))
    ;; Count neighbors
    ;; pmap is *much* slower here...
    ;; basically sequential since it has to lock the hash table,
    ;; with the added overhead of actually locking it each time
    ;; (lparallel:pmap nil (lambda (pt)
    ;;                   (loop
    ;;        :for a :in '(-1 0 1)
    ;;        :do
    ;;           (loop
    ;;             :for b :in '(-1 0 1)
    ;;             :do
    ;;                (incf (gethash (2d-pt (+ (pt-x pt) a)
    ;;                                   (+ (pt-y pt) b))
    ;;                               counter
    ;;                               0)))))
    ;;                 pts)
    (loop
      :for pt :in pts
      :do
         (loop
           :for a :in '(-1 0 1)
           :do
              (loop
                :for b :in '(-1 0 1)
                :do
                   (incf (gethash (2d-pt (+ (pt-x pt) a)
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
  "Advance using the baseline life algorithm a certain number of times."
  (loop
    :for game = pts :then (iterate-baseline-life game)
    :for i :below times
    :finally (return game)))


(defun show-all-life-files-in-directory (&key
                                           (directory (car *game-file-dirs*))
                                           (filter uiop/pathname:*wild-file-for-directory*)
                                           (size-limit-in-bytes (* 100 1024)))
  ""
  (loop :for file-name :in (uiop:directory-files directory filter)
        :for fname = (concatenate 'string
                                  (pathname-name file-name)
                                  "."
                                  (pathname-type file-name))
        :for fsize = (with-open-file (stream file-name) (file-length stream))
        :for life = (when (< fsize size-limit-in-bytes)
                      (make-life fname))
        :when life
          :do
             (format t "~a (~a):~%" file-name fname)
             (hl:show-life life t)
             (format t "~a~%~%" (make-string 120 :initial-element #\#))))
