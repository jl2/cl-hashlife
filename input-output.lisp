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

(defun read-rle-stream (stream)
  "Read an RLE life stream and return a list of coordinates for the 'live' cells."
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
                     :collecting (2d-pt cur-x cur-y)
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

(defun game-bounds (pts)
  "Find the bounding box (minx, miny, maxx, maxy) coordinates of a list of points"
  (loop :for pt :in pts
        :minimizing (pt-x pt) :into min-x
        :minimizing (pt-y pt) :into min-y
        :maximizing (pt-x pt) :into max-x
        :maximizing (pt-y pt) :into max-y
        :finally (return (values min-x min-y max-x max-y))))

(defun write-rle-stream (stream pts comment)
  "Not implemented"
  (declare (ignorable stream pts comment))
  (format stream "# ~a~%" comment)
  (error "Not implemented")
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
  "Read a life 1.06 stream to a list of points."
  (loop
    :with header = (read-line stream nil nil)
    :for x = (read stream nil nil)
    :for y = (read stream nil nil)
    :while (and x y)
    :collect (2d-pt x y) :into pts
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
  "Write a life 1.06 format file to stream."
  (format stream "# ~a~%" comment)
  (loop :for pt :in (sort (copy-list pts) #'pt-<) :do
    (format stream "~a ~a~%" (pt-x pt) (pt-y pt)))
  pts)

(defun read-life-1.05-stream (stream)
  "Read a life 1.05 stream to a list of points."
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
              :collect (2d-pt this-x this-y))
        (incf this-y))
    ))

(defun read-cells-stream (stream)
  "Read a cells formatted file from stream to a list of points."
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
            :collect (2d-pt x y))))

(defun write-cells-stream (stream pts &optional (comment ""))
  "Write a cells formatted file to stream."

  (format stream "! ~a~%" comment)
  (multiple-value-bind (min-x min-y max-x max-y) (game-bounds pts)
    ;; (loop :for pt :in pts
    ;; :minimizing (pt-x pt) :into min-x
    ;; :maximizing (pt-x pt) :into max-x
    ;; :minimizing (pt-y pt) :into min-y
    ;; :maximizing (pt-y pt) :into max-y
    ;; :finally (return (values min-x min-y max-x max-y)))
    (let ((pts (sort (copy-list pts) #'pt-<)))
      (loop
        :for y :from min-y :to max-y :do
          (loop :for x :from min-x :to max-x
                :for elem = (2d-pt x y)
                :do
                   (cond
                     ((find elem pts :test #'pt-=)
                      (setf pts (delete elem pts :test #'pt-=))
                      (format stream "O"))
                     (t (format stream "."))))
          (format stream "~%"))))
  pts)


(defun find-game-file (fname)
  "Resolve a game file name.  If fname is a path or (probe-file fname), then return it.
Otherwise look for a file named fname in each of the *game-file-dirs*."
  (ctypecase fname
    (string
     (if (probe-file fname)
         ;; It's a specific file, so return it.
         fname

         ;; Not a specific file, so search in *game-file-dirs*
         (loop
           :for path :in *game-file-dirs*
           :do
              
              (if (pathname-type fname)
                  ;; If it has an extension, probe for it
                  (when-let (the-file (probe-file (merge-pathnames fname path)))
                    (return-from find-game-file (merge-pathnames fname path)))
                  ;; Otherwise search for it
                  (return-from find-game-file  (car (uiop:directory-files path (concatenate 'string fname ".*")))))

           :finally (error "Could not find ~a" fname)))) ;; Doesn't exist, so error out.
    (pathname
     ;; It's a path, so return it.
     (probe-file fname))))


(defun find-reader (path)
  "Find a reader function for a specific filename."
  (let ((readers `(("life" . ,#'read-life-1.06-stream)
                   ("lif" . ,#'read-life-1.05-stream)
                   ("cells" . ,#'read-cells-stream)
                   ("rle" . ,#'read-rle-stream))))
    (assoc-value readers
                 (string-downcase (pathname-type path))
                 :test #'string= )))

(defun read-game-file (file-name-or-path)
  "Resolves file-name-or-path and reads the file using the correct stream reader."
  (let ((path (find-game-file file-name-or-path)))
    (with-open-file (stream path)
      (funcall (find-reader path) stream))))

(defun write-game-file (pts file-name &optional (comment "Written by cl-hashlife"))
  "Writes a .life or .cells file from pts"
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

