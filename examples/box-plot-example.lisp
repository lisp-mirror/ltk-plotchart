;; Translation of plotdemos8.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "box-plot-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400))
                 (box-plot (chart:create-box-plot canvas '(0 40 5) '(A B C D E F))))
            (grid canvas 0 0)

            (chart:plot box-plot 'data 'A '(0 1 2 5 7 1 4 5 0.6 5 5.5))
            (chart:plot box-plot 'data 'C '(2 2 3 6 1.5 3))
            (chart:plot box-plot 'data 'E '(2 3 3 4 7 8 9 9 10 10 11 11 11 14 15 17 17 20 24 29))))
