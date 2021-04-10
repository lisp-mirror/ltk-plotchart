(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "radial-chart-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400
                                        :background :white))
                 (radial-chart (chart:create-radial-chart canvas 
                                                          '("Mon" "Tue" "Wed" "Thu" "Fri")
                                                          10.0)))
            (grid canvas 0 0)

            (chart:plot radial-chart '(5 8 4 7 10) :green 2)
            (chart:plot radial-chart '(2 4 1 3 5) :blue 2)))

