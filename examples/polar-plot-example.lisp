(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "polar-plot-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 300 
                                        :background :white))
                 (polar-plot (chart:create-polar-plot canvas '(3.0 1.0))))
            (grid canvas 0 0)

            (chart:data-config polar-plot "line" :colour :blue
                               :type :both :symbol :cross)

            (loop for r from 0 to 3.0 by 0.1
                  for a from 0 by 24
                  do (chart:plot polar-plot "line" r a))
            
            (chart:draw-label-dot polar-plot 2 60 "Mark")))
