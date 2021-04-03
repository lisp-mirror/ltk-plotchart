;; Translation of plotdemos4.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plotdemos4.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400 :background :white))
                 (window2 (make-instance 'toplevel 
                                         :master *tk*
                                         :title "plotdemos4.lisp - t"))
                 (canvas2 (make-instance 'canvas
                                         :master window2
                                         :width 300 :height 200 :background :white))
                 (canvas3 (make-instance 'canvas
                                         :master window2
                                         :width 300 :height 200 :background :white))
                 (canvas4 (make-instance 'canvas
                                         :master window2
                                         :width 300 :height 200 :background :white)))
            (pack canvas :fill :both)
            (pack canvas2 :fill :both)
            (pack canvas3 :fill :both)
            (pack canvas4 :fill :both)

            ; 3D barchart
            (let ((chart (chart:create-3d-bar-chart canvas '(-200.0 900.0 100.0) 7)))
              (dolist (pair '(("red" 765) ("green" 234) ("blue" 345) ("yellow" 321)
                                          ("magenta" 567) ("cyan" -123) ("white" 400)))
                (chart:plot chart (first pair) (second pair) (first pair)))
              (chart:title-text chart "3D bars")
              (chart:balloon chart 1.2 100 "Arrow pointing\\nat second bar" :south-east))

            ; Three styles of radial charts
            (dolist (pair (list (list "lines" canvas2) 
                                (list "cumulative" canvas3) 
                                (list "filled" canvas4)))
              (let ((chart (chart:create-radial-chart (second pair)
                                                      '(A B "LongerName" C D)
                                                      10.0
                                                      (first pair))))
                (chart:plot chart '(1 2 3 4 3) :green 2)
                (chart:plot chart '(4 5 0 1 4) :red 3)
                (chart:title-text chart 
                                  (uiop:strcat "Sample of a radial chart - style: " 
                                               (first pair)))))))
