(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "windrose-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 500 :height 300 :background :white))
                 (windrose (chart:create-windrose canvas '(50 10) :num-sectors 5))
                 )
            (grid canvas 0 0)

            (chart:plot windrose '(10 20 30 25 15) :green)

            ))
