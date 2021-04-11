; Translation of test_ternary.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "test_ternary.lisp")
          (let* ((canvas (make-instance 'canvas :width 500 :height 500))
                 (ternary-diagram (chart:create-ternary-diagram canvas)))
            (pack canvas)

            (chart:corner-text ternary-diagram "Component A" "Component B" "Component C")
            (chart:plot ternary-diagram "data" 50.0 25.0 25.0 "1")
            (chart:plot ternary-diagram "data" 20.0 25.0 55.0 "2" :e)
            (chart:draw-line ternary-diagram "data" '((0.0 80.0 20.0) (10.0 20.0 70.0)))

            (chart:data-config ternary-diagram :area :colour :green :smooth t)
            (chart:draw-filled-polygon ternary-diagram :area '((0.0 70.0 30.0) (10.0 20.0 70.0)
                                                                               (0.0 0.0 100.0)))
            (chart:plot ternary-diagram "area1" 0.0 70.0 30.0 "")
            (chart:plot ternary-diagram "area1" 10.0 20.0 70.0 "")
            (chart:plot ternary-diagram "area1" 0.0 0.0 100.0 "")

            (chart:draw-ticklines ternary-diagram :grey)
            ))

