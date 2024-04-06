;; points.lisp

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

(declaim (inline make-life-point
                 pt
                 pt-<
                 pt-+
                 pt--
                 pt-=
                 even-pt
                 right-pt
                 corner-pt
                 pt-x
                 pt-y
                 pt-in-box
                 p))

(defstruct (life-point (:conc-name pt-) )
  "A game of life cell."
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum)
  (gray 0 :type (or fixnum rational qtnode)))


(defun 2d-pt (x y &optional (gray 1))
  "Convenience function for creating a 2d point."
  (declare (type fixnum x y)
           (type (or rational fixnum qtnode) gray))
  (make-life-point :x x
                   :y y
                   :z 0
                   :gray gray))

(defun 3d-pt (x y z &optional (gray 1))
  "Convenience function for creating a 3d point."
  (declare (type fixnum x y z)
           (type (or rational fixnum qtnode) gray))
  (make-life-point :x x
                   :y y
                   :z z
                   :gray gray))

(defun pt-< (a b)
  "Determine if point a is 'less' than b - if"
  (declare (type life-point a b))
  (or (< (pt-z a) (pt-z b))
      (if (= (pt-z a) (pt-z b))
          (or  (< (pt-y a) (pt-y b))
               (if (= (pt-y a) (pt-y b))
                   (< (pt-z a) (pt-z b))
                   nil))
          nil)))

(defun pt-in-box (pt lower upper)
  "Determine if pt is in the bounding points lower and upper"
  (declare (type life-point pt lower upper))
  (and (< (pt-x pt) (pt-x upper))
       (< (pt-y pt) (pt-y upper))
       (< (pt-z pt) (pt-z upper))
       (> (pt-x pt) (pt-x lower))
       (> (pt-y pt) (pt-y lower))
       (> (pt-z pt) (pt-z lower))))

(defun pt-= (a b)
  "Determine if a and b are equal."
  (declare (type life-point a b))
  (and (= (pt-x a) (pt-x b))
       (= (pt-y a) (pt-y b))
       (= (pt-z a) (pt-z b))))

(defun pt-+ (a b)
  "Add a to b."
  (make-life-point :x (+ (pt-x a) (pt-x b))
                   :y (+ (pt-y a) (pt-y b))
                   :z (+ (pt-z a) (pt-z b))
                   :gray (pt-gray a)))

(defun pt-- (a b)
  "Subract b from a."
  (make-life-point :x (- (pt-x a) (pt-x b))
                   :y (- (pt-y a) (pt-y b))
                   :z (- (pt-z a) (pt-z b))
                   :gray (pt-gray a)))

(defun even-pt (pt)
  "Return the point on even coordinates"
  (make-life-point :x (- (pt-x pt) (logand (pt-x pt) 1))
                   :y (- (pt-y pt) (logand (pt-y pt) 1))
                   :z (- (pt-z pt) (logand (pt-z pt) 1))
                   :gray (pt-gray pt)))

(defun right-pt (pt)
  "Return the point coordinate right of pt."
  (make-life-point :x (1+ (pt-x pt))
                   :y (pt-y pt)
                   :gray (pt-gray pt)))

(defun down-pt (pt)
  "Return the point coordinate below pt."
  (make-life-point :x (pt-x pt)
                   :y (1+ (pt-y pt))
                   :gray (pt-gray pt)))

(defun corner-pt (pt)
  "Return the point coordinate down and right of this one.  Equivalent to (right-pt (down-pt pt))."
  (make-life-point :x (1+ (pt-x pt))
                   :y (1+ (pt-y pt))
                   :gray (pt-gray pt)))

