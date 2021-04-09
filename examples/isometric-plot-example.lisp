(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "isometric-plot-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400
                                        :background :white))
                 (iso-plot (chart:create-isometric-plot canvas '(0.0 100.0) '(0.0 200.0) :noaxes)))
            (grid canvas 0 0)

            (chart:draw-rectangle iso-plot 10.0 10.0 50.0 50.0 :green)
            (chart:draw-filled-rectangle iso-plot 20.0 20.0 40.0 40.0 :red)
            (chart:draw-filled-circle iso-plot 70.0 70.0 40.0 :yellow)
            (chart:draw-circle iso-plot 70.0 70.0 42.0)))
