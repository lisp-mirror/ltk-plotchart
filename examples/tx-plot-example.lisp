;; Translation of testtxplot.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "tx-plot-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 500 :height 200 :background :white))
                 (tx-plot (chart:create-tx-plot canvas 
                                                '("2001-01-01" "2015-01-01" 1461)
                                                '(-10.0 20.0 10.0))))
            (grid canvas 0 0)

            (chart:data-config tx-plot "min" :colour 'red)
            (chart:data-config tx-plot "max" :colour 'blue)

            (chart:x-text tx-plot "Time")
            (chart:v-text tx-plot "Temperature")

            (chart:legend-config tx-plot :position :bottom-right)
            (chart:legend tx-plot "min" "Minimum Temperature")
            (chart:legend tx-plot "max" "Maximum Temperature")

            (chart:plot tx-plot "min" "2001-01-01" -3.0)
            (chart:plot tx-plot "min" "2004-01-01" 4.0)
            (chart:plot tx-plot "min" "2007-01-01" 2.0)
            (chart:plot tx-plot "min" "2010-01-01" -1.0)
            (chart:plot tx-plot "min" "2013-01-01" 2.0)
            (chart:plot tx-plot "min" "2014-01-01" 5.0)

            (chart:plot tx-plot "max" "2001-01-01" 10.0)
            (chart:plot tx-plot "max" "2004-01-01" 12.0)
            (chart:plot tx-plot "max" "2007-01-01" 8.0)
            (chart:plot tx-plot "max" "2010-01-01" 6.0)
            (chart:plot tx-plot "max" "2013-01-01" 15.0)
            (chart:plot tx-plot "max" "2014-01-01" 18.0)


            ))

