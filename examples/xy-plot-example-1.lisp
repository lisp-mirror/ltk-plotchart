(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(setf *debug-tk* t)

(with-ltk ()
          (wm-title *tk* "xy-plot-example-1.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400 :background :white))
                 (xy (chart:create-xy-plot canvas '(-10 10 2) '(-100 100 20))))
            (grid canvas 0 0)

            (chart:title-text xy "Two Functions")
            (chart:x-text xy "Input x")
            (chart:v-text xy "Output y")

            (chart:data-config xy "square" :colour 'blue)
            (chart:data-config xy "cube" :colour 'green)

            (chart:legend-config xy :position 'bottom-right)
            (chart:legend xy "square" "x*x")
            (chart:legend xy "cube" "x*x*x")

            (dotimes (n 20)
              (let ((i (- n 10)))
                (chart:plot xy "square" i (* i i))
                (chart:plot xy "cube" i (* i i i))))

            (chart:balloon xy 0 0 "crossover point" :north-west)
            (chart:plaintext-config xy :text-colour :red)
            (chart:plaintext xy 6 80 "diverging" :north-west)
            (chart:save-plot xy "xy-plot.ps")
            ))


