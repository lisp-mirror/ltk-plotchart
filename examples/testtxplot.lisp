;; Translation of testtxplot.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "testtxplot.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 200 :background :white))
                 (tx-plot (chart:create-tx-plot canvas 
                                                '("2006-01-01" "2007-01-01" 120)
                                                '(0.0 100.0 20.0))))
            (grid canvas 0 0)

            (chart:data-config tx-plot "series1" :colour 'red)
            (chart:data-config tx-plot "series2" :colour 'blue)

            (chart:x-text tx-plot "Time")
            (chart:v-text tx-plot "Data")
            (chart:x-ticklines tx-plot)

            (chart:plot tx-plot "series1" "2006-02-01" 10.0)
            (chart:plot tx-plot "series1" "2006-02-11" 50.0)
            (chart:plot tx-plot "series1" "2006-03-01" 50.0)
            (chart:plot tx-plot "series1" "2006-07-01" 40.0)
            (chart:plot tx-plot "series1" "2006-08-21" 20.0)
            (chart:plot tx-plot "series1" "2006-08-22" 1.0)
            (chart:plot tx-plot "series1" "2006-12-11" 78.0)

            (chart:plot tx-plot "series2" "2006-03-01" 110.0)
            (chart:plot tx-plot "series2" "2006-04-11" 50.0)
            (chart:plot tx-plot "series2" "2006-07-28" 20.0)
            (chart:plot tx-plot "series2" "2006-10-21" 99.0)
            (chart:plot tx-plot "series2" "2006-11-22" 1.0)
            (chart:plot tx-plot "series2" "2006-12-31" 78.0)

            ))

