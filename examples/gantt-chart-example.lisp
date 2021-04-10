(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "gantt-chart-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400
                                        :background :white))
                 (gantt (chart:create-gantt-chart canvas "1 march 2021" "10 april 2021"
                                                  :num-items 5
                                                  :ylabel-width 15))
                 (zero (chart:milestone gantt "Vacation Start" "20 march 2021" :green))
                 (one (chart:task gantt "ltk" "1 march 2021" "15 march 2021" 70))
                 (two (chart:task gantt "chart" "15 march 2021" "31 march 2021" 30))
                 (three (chart:task gantt "ltk-examples" "7 march 2021" "31 march 2021" 50))
                 (four (chart:task gantt "documentation" "15 march 2021" "31 march 2021" 10)))
            (grid canvas 0 0)

            (chart:connect gantt one two)
            (chart:connect gantt three four)
            (chart:draw-vertical-line gantt "1 mar" "1 march 2021")
            (chart:draw-vertical-line gantt "8 mar" "8 march 2021")
            (chart:draw-vertical-line gantt "15 mar" "15 march 2021")
            (chart:draw-vertical-line gantt "22 mar" "22 march 2021")
            (chart:draw-vertical-line gantt "1 apr" "1 april 2021")

            (chart:title-text gantt "Learning LTk"))

          )
