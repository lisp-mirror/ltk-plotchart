;; Translation of testtxplot.tcl

(require "asdf")
(require "ltk-plotchart")
(use-package :ltk)
(use-package :ltk-plotchart)

(with-ltk ()
          (wm-title *tk* "testtxplot.lisp")
          (let* ((c (make-instance 'canvas :width 400 :height 200 :background :white))
                 (tx (make-instance 'tx-plot :canvas c 
                                    :timeaxis '("2006-01-01" "2007-01-01" 120)
                                    :xaxis '(0.0 100.0 20.0))))

            (tx-plot-dataconfig tx "series1" :colour 'red)
            (tx-plot-dataconfig tx "series2" :colour 'blue)

            (chart-xtext tx "Time")
            (chart-vtext tx "Data")
            (chart-xticklines tx)

            (tx-plot-plot tx "series1" "2006-02-01" 10.0)
            (tx-plot-plot tx "series1" "2006-02-11" 50.0)
            (tx-plot-plot tx "series1" "2006-03-01" 50.0)
            (tx-plot-plot tx "series1" "2006-07-01" 40.0)
            (tx-plot-plot tx "series1" "2006-08-21" 20.0)
            (tx-plot-plot tx "series1" "2006-08-22" 1.0)
            (tx-plot-plot tx "series1" "2006-12-11" 78.0)

            (tx-plot-plot tx "series2" "2006-03-01" 110.0)
            (tx-plot-plot tx "series2" "2006-04-11" 50.0)
            (tx-plot-plot tx "series2" "2006-07-28" 20.0)
            (tx-plot-plot tx "series2" "2006-10-21" 99.0)
            (tx-plot-plot tx "series2" "2006-11-22" 1.0)
            (tx-plot-plot tx "series2" "2006-12-31" 78.0)

            (chart-balloon tx "2006-05-06" 60.0 "Aha" :n)
            (chart-plaintext tx "2006-11-06" 50.0 "Aha")

            (pack c)

            ))

