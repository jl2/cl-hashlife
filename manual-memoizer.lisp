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

(defstruct (manual-memoizer (:conc-name mm-))
  (hash-table (make-hash-table :test 'equal
;;                               #+sbcl :weakness #+sbcl :value
                               ))
  (hash-function (lambda (x) x))
  (hit-count 0 :type fixnum)
  (miss-count 0 :type fixnum)
  (call-count 0 :type fixnum)
  (enabled nil :type (or t nil)))

(declaim (inline mm-get mm-reset mm-enable mm-disable mm-add mm-hash))

(defun mm-hash-table-size (mm)
  (hash-table-count (mm-hash-table mm)))

(defun mm-get (mm &rest params)
  (incf (mm-call-count mm))
  (cond
    ((mm-enabled mm)
     (multiple-value-bind (val foundp)
         (gethash (apply (mm-hash-function mm) params) (mm-hash-table mm) nil)
       (if foundp
           (incf (mm-hit-count mm))
           (incf (mm-miss-count mm)))
       (values val foundp)))

    (t
     (values nil nil))))

(defun mm-reset (mm)
  (clrhash (mm-hash-table mm))
  (setf (mm-hit-count mm) 0
        (mm-miss-count mm) 0
        (mm-call-count mm) 0))

(defun mm-enable (mm)
  (setf (mm-enabled mm) t))

(defun mm-disable (mm)
  (setf (mm-enabled mm) nil))

(defun mm-add (mm params value)
  (cond
    ((mm-enabled mm)
     (let ((hash-value (apply (mm-hash-function mm) params)))
       (setf (gethash hash-value (mm-hash-table mm))
             value)
       (gethash hash-value (mm-hash-table mm))))
    (t
     value)))

(defun mm-hash (mm &rest params)
  (apply (mm-hash-function mm) params))
