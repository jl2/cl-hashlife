;; cl-hashlife.asd

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

(asdf:defsystem #:cl-hashlife
  :description "Describe cl-hashlife here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:command-line-arguments
               #:uiop
               #:alexandria
               #:str
               #:3d-vectors
               #:lparallel
               #:svg
               #-sbcl #:cl-unicode)
  :components ((:file "package")
               (:file "manual-memoizer")
               (:file "input-output")
               (:file "points")

               (:file "hashlife")
               (:file "utilities")
               (:file "visualization")
               (:file "main"))

  :in-order-to ((test-op (test-op cl-hashlife.test))))
