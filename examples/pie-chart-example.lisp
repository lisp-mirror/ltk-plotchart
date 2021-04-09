(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "pie-chart-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 500 :height 300 :background :white))
                 (pie-chart (chart:create-pie-chart canvas))
                 )
            (grid canvas 0 0)

            (chart:title-text pie-chart "Books Read per Category")
            (chart:plot pie-chart '(("Computing" 3)
                                    ("Fiction" 10)
                                    ("Technical" 25)))

            (chart:explode pie-chart 0)))
