(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "histogram-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400
                                        :background :white))
                 (histogram (chart:create-histogram canvas '(0 10 1) '(0 100 5))))
            (grid canvas 0 0)

            (chart:title-text histogram "Example Histogram")
            (chart:v-text histogram "Cumulative Frequency")
            (chart:x-text histogram "Event")

            (chart:data-config histogram "data" 
                               :colour :green
                               :style :filled
                               :fillcolour :green)

            (loop for i from 2 to 10 by 2
              do (chart:plot-cumulative histogram "data" i (random 30))))

          )

