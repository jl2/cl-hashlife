;; writers.lisp
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
(defpackage :cl-hashlife.test.writers
  (:use :cl
        :fiveam
        :alexandria
        :cl-hashlife))

(in-package :cl-hashlife.test.writers)

(def-suite :cl-hashlife-writers)
(in-suite :cl-hashlife-writers)

(test write-life-1.06
      (let* ((glider
               (list (hl::2d-pt -1  1)
                     (hl::2d-pt 1  0)
                     (hl::2d-pt 0  -1)
                     (hl::2d-pt 1  1)
                     (hl::2d-pt 0  1)))
             (str (with-output-to-string (outs)
                    (hl::write-life-1.06-stream outs glider "test-write-life-1.06"))))
        (is (string= str
                     "# test-write-life-1.06
0 -1
1 0
-1 1
0 1
1 1
"))))

(test write-cells
      (let* ((glider (list (hl::2d-pt -1  1)
                           (hl::2d-pt 1  0)
                           (hl::2d-pt 0  -1)
                           (hl::2d-pt 1  1)
                           (hl::2d-pt 0  1)))
             (str (with-output-to-string (outs)
                    (hl::write-cells-stream outs glider "test-write-cells"))))
        (is (string= str
                     "! test-write-cells
.O.
..O
OOO
"))))
