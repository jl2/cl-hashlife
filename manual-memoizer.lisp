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

(defun mm-hash-table-size (mm)
  (hash-table-count (mm-hash-table mm)))

(defun mm-get (mm &rest params)
  (with-slots (hash-table hash-function enabled call-count hit-count miss-count) mm
    (incf call-count)
    (cond
      (enabled
       (multiple-value-bind (val foundp)
           (gethash (apply hash-function params) hash-table nil)
         (if foundp
             (incf hit-count)
             (incf miss-count))
         (values val foundp)))

      ((not enabled)
       (values nil nil)))))

(defun mm-reset (mm)
  (with-slots (hash-table call-count hit-count miss-count) mm
    (clrhash hash-table)
    (setf hit-count 0
          miss-count 0
          call-count 0)))

(defun mm-enable (mm)
  (with-slots (enabled) mm
    (setf enabled t)))

(defun mm-disable (mm)
  (with-slots (enabled) mm
    (setf enabled nil)))

(defun mm-add (mm params value)
  (with-slots (hash-table
               hash-function
               miss-count
               enabled)
      mm
    (cond
      (enabled
       (let ((hash-value (apply hash-function params)))
         (setf (gethash hash-value hash-table)
               value)
         (gethash hash-value hash-table)))
      ((not enabled)
       value))))

(defun mm-hash (mm &rest params)
  (apply (mm-hash-function mm) params))
