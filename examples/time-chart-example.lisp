(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "time-chart-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 550 :height 300
                                        :background :white))
                 (time-chart (chart:create-time-chart canvas "1 january 2004" "31 december 2004" 
                                                      :num-items 4)))
            (grid canvas 0 0)

            (chart:title-text time-chart "Seasons (northern hemisphere)")

            (chart:period time-chart "Spring" "1 march 2004" "1 june 2004" :green)
            (chart:period time-chart "Summer" "1 june 2004" "1 september 2004" :yellow)
            (chart:add-period time-chart "21 september 2004" "21 october 2004" :blue)
            (chart:draw-vertical-line time-chart "1 jan" "1 january 2004")
            (chart:draw-vertical-line time-chart "1 apr" "1 april 2004" :colour :lime)
            (chart:draw-vertical-line time-chart "1 jul" "1 july 2004")
            (chart:draw-vertical-line time-chart "1 oct" "1 october 2004")
            (chart:milestone time-chart "Longest day" "21 july 2004")
            (chart:add-milestone time-chart "21 december 2004")))

