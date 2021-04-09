(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "right-axis-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400
                                        :background :white))
                 (xy-plot (chart:create-xy-plot canvas '(0 10 1) '(0 100 10)))
                 (right-axis (chart:create-right-axis canvas '(0 3.0 0.5))))
            (grid canvas 0 0)

            (chart:v-text xy-plot "y = x*x")
            (chart:v-text right-axis "y = sqrt(x)")

            (chart:data-config xy-plot "squares" :colour :blue)
            (chart:data-config right-axis "roots" 
                               :colour :green
                               :type :both
                               :symbol :upfilled)

            (dotimes (x 10)
              (chart:plot xy-plot "squares" x (* x x))
              (chart:plot right-axis "roots" x (sqrt x)))

            ))

