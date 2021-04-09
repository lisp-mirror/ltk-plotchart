(require 'asdf)    
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "bar-chart-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400 
                                        :background :white))
                 (bar-chart (chart:create-bar-chart canvas 
                                                    '(2018 2019 2020 2021)
                                                    '(0 30 5)
                                                    3)))
            (grid canvas 0 0)

            (chart:title-text bar-chart "Book Reading History")
            (chart:x-text bar-chart "Year")
            (chart:v-text bar-chart "Number of Books")

            (chart:config bar-chart :show-values t)
            (chart:plot bar-chart :person-1 '(3 6 9 12) :blue)
            (chart:plot bar-chart :person-2 '(10 7 11 15) :red)
            (chart:plot bar-chart :person-3 '(25 18 21 22) :green)

            (chart:legend bar-chart :person-1 "Mary")
            (chart:legend bar-chart :person-2 "Peter")
            (chart:legend bar-chart :person-3 "Shalini")
            ))

