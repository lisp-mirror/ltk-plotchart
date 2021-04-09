(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "spiral-pie-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 500 :height 300 :background :white))
                 (spiral-pie (chart:create-spiral-pie canvas))
                 )
            (grid canvas 0 0)

            (chart:colours spiral-pie :yellow :blue :red) 

            (chart:title-text spiral-pie "Books Read per Category")
            (chart:plot spiral-pie '(("Computing" 3)
                                    ("Fiction" 10)
                                    ("Technical" 25)))))
