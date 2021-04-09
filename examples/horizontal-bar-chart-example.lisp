(require 'asdf)    
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "horizontal-bar-chart-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400 
                                        :background :white))
                 (bar-chart (chart:create-horizontal-bar-chart canvas 
                                                               '(0 50 5)
                                                               '(2018 2019 2020 2021)
                                                               :stacked)))
            (grid canvas 0 0)

            (chart:title-text bar-chart "Book Reading History")
            (chart:x-text bar-chart "Number of Books")
            (chart:v-text bar-chart "Year")

            (chart:plot bar-chart :type-1 '(3 6 9 12) :blue)
            (chart:plot bar-chart :type-2 '(10 7 11 15) :red)
            (chart:plot bar-chart :type-3 '(25 18 21 22) :green)

            (chart:legend-config bar-chart 
                                 :position :bottom-right
                                 :spacing 12)
            (chart:legend bar-chart :type-1 "Computing")
            (chart:legend bar-chart :type-2 "Fiction")
            (chart:legend bar-chart :type-3 "Technical")
            ))

