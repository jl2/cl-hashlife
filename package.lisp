;; package.lisp

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

(defpackage :cl-hashlife

  (:nicknames #:hl)

  (:use #:cl #:alexandria)
  (:export #:spatial-tree-element-type
           #:quadtree-node
           #:make-quad-tree-node
           #:make-hash-life
           #:hl-insert
           #:show-life-game
           #:read-game-file
           #:write-game-file

           #:read-life-1.05-stream
           #:read-life-1.06-stream
           #:write-life-stream

           #:read-cells-stream
           #:write-cells-stream

           #:read-rle-stream
           #:write-rle-stream

           #:iterate-baseline-life
           #:construct
           #:expand
           #:successor
           #:qtnode
           #:maybe-qtnode
           #:life
           #:center
           #:get-zero
           #:life-4x4
           #:*on*
           #:*off*
           #:iterate-game-of-life
           
           ))
