;; package.lisp
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
(defpackage :cl-hashlife.test
  (:use :cl
        :fiveam
        :alexandria
        :cl-hashlife))

(in-package :cl-hashlife.test)

(def-suite :cl-hashlife)
(in-suite :cl-hashlife)

(test read-life-1.06
  (is (equal (hl:read-game-file "glider.life")
             '((-1 . 1) (0 . -1) (0 . 1) (1 . 0) (1 . 1)))))

(test read-cells
  (is (equal (hl:read-game-file "glider.cells" )
             '((1 . 0) (2 . 1) (0 . 2) (1 . 2) (2 . 2)))))

(test read-rle
  (is (equal (hl:read-game-file "glider.rle")
             '((1 . 0) (2 . 1) (0 . 2) (1 . 2) (2 . 2)))))
