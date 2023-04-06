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


(defun show-version ()
  (format t "Hashlife version: ~a~%"
          #.(asdf:component-version (asdf:find-system :cl-hashlife nil))))

(defparameter +command-line-spec+
  '((("output" #\o )
     :type string
     :optional nil
     :documentation "The output directory for images.")
    (("input" #\i)
     :type string
     :optional nil
     :documentation #.(format nil "The game of life file name from one of these directories: ~{ ~a~}~%" *game-file-dirs*) )
    (("type" #\t)
     :type string
     :initial-value "hashlife"
     :optional t
     :documentation #.(format nil "Choose type of output.  Default is 'hashflife' to run hashlife. 'baseline' to animate the naive algorithm, or 'qt' to animate the hashlife successor hash-table."))
    (("frame-count" #\f)
     :type integer
     :initial-value 10
     :optional t
     :documentation #.(format nil "Number of frames to animate. Default is 10"))
    (("depth" #\d)
     :type integer
     :initial-value 8
     :optional t
     :documentation #.(format nil "Depth of quadtree to show.  Only used by 'qt' type.. Default is 8"))
    (("width" #\w)
     :type integer
     :initial-value 1024
     :optional t
     :documentation #.(format nil "Width of images to create.  Default is 1024"))
    (("verbose")
     :type boolean
     :optional t
     :documentation "Extra output.  Currently not used...")
    (("help" #\h #\?)
     :type boolean
     :optional t
     :documentation "--help -h -?, good practice to have")
    (("version" #\V)
     :type boolean
     :optional t
     :documentation "--version or -V, you get the idea")))


(defun hashlife-main-function (rest-args &key
                                        (output)
                                        (input)
                                        (type "hashlife")
                                        (frame-count 10)
                                        (depth 8)
                                        (width 1024)
                                        verbose
                                        help
                                        version)
  (declare (ignorable rest-args verbose))
  (cond
    (help
     (command-line-arguments:show-option-help +command-line-spec+ :sort-names t))
    (version
     (show-version))
    ((string= (string-downcase type)
              "successor")
     (animate-hashlife-successor output
                                 (pathname-name input)
                                 (make-hashlife input)
                                 frame-count
                                 :depth depth
                                 :width width))
    ((string= (string-downcase type)
              "qt")
     (animate-hashlife-qt output
                          (pathname-name input)
                          (make-hashlife input)
                          frame-count
                          :depth depth
                          :width width))

    ((string= (string-downcase type)
              "baseline")
     (animate-life output
                   (pathname-name input)
                   (make-life input)
                   frame-count
                   :level 0
                   :width width
                   :height width))
    ((string= (string-downcase type)
              "hashlife")
     (animate-life output
                   (pathname-name input)
                   (make-hashlife input)
                   frame-count
                   :level 0
                   :width width
                   :height width)))
  (uiop:quit))

(defun main (args)
  (command-line-arguments:handle-command-line 
   ;; the spec as above, or prepared with prepare-command-line-options-specification
   +command-line-spec+
   ;; the function to call with the arguments as parsed
   'hashlife-main-function
   ;; the arguments to parse
   :command-line (rest args)
   ;; the program name to use in case of an error message
   :name "hashlife"
   ;; the number of mandatory positional arguments for this command (default: 0)
   :positional-arity 0
   ;; What to do with the rest of the positional arguments.
   ;; T means pass the list of the rest of the command-line-arguments as one lisp argument.
   ;; NIL means ignore it. A keyword means pass this rest as a keyword argument.
   :rest-arity t))
  
