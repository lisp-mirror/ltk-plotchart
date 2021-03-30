;; Translation of test-histogram.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)
(use-package :ltk-plotchart)

(with-ltk ()
          (wm-title *tk* "test-histogram.lisp")
          (let* ((c (make-instance 'canvas :width 400 :height 300 :background :white))
                 (h (make-instance 'histogram :canvas c :xaxis '(0 100 20) :yaxis '(0 50 10))))

            (histogram-dataconfig h "data"
                                  :style :filled
                                  :fillcolour :cyan
                                  :width 2 
                                  :colour :blue)

            (histogram-plot h "data" 0.0 10.0)
            (histogram-plot h "data" 20.0 10.0)
            (histogram-plot h "data" 40.0 3.0)
            (histogram-plot h "data" 45.0 6.0)
            (histogram-plot h "data" 55.0 26.0)
            (histogram-plot h "data" 67.0 24.0)

            (pack c)))
