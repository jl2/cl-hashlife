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

  (:use #:cl #:alexandria #:3d-vectors)
  (:export
   #:main


   #:make-context
   #:to-svg
   #:qt-to-svg
   #:animate-life
   #:animate-hashlife-qt
   #:show-hashlife-successors
   #:show-successor
   #:show-advance
   #:show-all-life-files-in-directory
   #:make-viz-context
   #:show-join
   #:show-inner-successors

   #:game-bounds
   #:make-life
   #:make-hashlife
   #:baseline-advance

   #:node-size

   ;; Look for game file in game-files directory
   ;; of cl-hashlife package directory and guess
   ;; file type by extension
   #:read-game-file
   #:write-game-file

   ;; No write support yet for these two
   #:read-life-1.05-stream
   #:read-rle-stream

   ;; Read/write .life 1.06
   #:read-life-1.06-stream
   #:write-life-1.06-stream

   ;; Read/write .cells
   #:read-cells-stream
   #:write-cells-stream

   ;; O(n) life using lists of (x . y) coordinates
   #:iterate-baseline-life

   #:2d-pt
   #:3d-pt
   #:make-pt
   #:pt-gray
   #:pt-x
   #:pt-y
   #:pt-=
   #:pt-+

   ;; Hashlife functions translated from this Python:
   ;; https://github.com/johnhw/hashlife
   #:show-life
   #:show-life-iteration
   #:show-side-by-side
   #:align
   #:same-pattern
   #:baseline
   #:get-by-address
   #:a
   #:b
   #:c
   #:d
   #:k
   #:n
   #:construct
   #:expand
   #:for-each-cell
   #:find-bounds
   #:successor
   #:qtnode
   #:pad
   #:is-padded
   #:q-join
   #:inner
   #:ffwd
   #:advance
   #:maybe-qtnode
   #:life
   #:crop
   #:center
   #:get-zero
   #:life-4x4
   #:*on*
   #:*off*

   #:q-k
   #:q-n
   #:q-a
   #:q-b
   #:q-c
   #:q-d

   #:testpat2

   #:*join-memo*
   #:*successor-memo*
   #:*zero-memo*

   #:make-manual-memoizer
   #:mm-enabled
   #:mm-calls
   #:mm-reset
   #:mm-enable
   #:mm-disable
   #:mm-get
   #:mm-add
   #:mm-hash
   #:mm-hits
   #:mm-misses
   #:mm-hash-table-size
   #:mm-hash-function
   #:mm-stats
   #:mm-reset-all
   ))
