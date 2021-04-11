(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "table-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 600 :height 400
                                        :background :white))
                 (table-chart (chart:create-table-chart 
                                canvas 
                                '("Year" "Computing" "Fiction" "Technical")
                                :widths 80)))
            (grid canvas 0 0)

            (chart:title-text table-chart "Book Reading History")

            (chart:separator table-chart)
            (chart:add-row table-chart '("2018" 3 10 25))
            (chart:add-row table-chart '("2019" 6 7 18))
            (chart:add-row table-chart '("2020" 9 11 21))
            (chart:add-row table-chart '("2021" 12 15 22))

            (chart:separator table-chart)
            (chart:cell-configure table-chart :colour :blue :font "Helvetica 11 bold")
            (chart:add-row table-chart '("Totals" 30 43 86))
            ))

