; Translation of plotdemos11.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plotdemos11.lisp")

          ; log x plot of y=log(x)
          (let* ((canvas (make-instance 'canvas :background :white))
                 (logx-y (chart:create-logx-y-plot canvas '(1 1000 "") '(0 5 1))))
            (pack canvas)

            (dolist (x '(1 2 5 10 20 50 100 200 500 1000))
              (chart:plot logx-y "series1" x (log x)))

            (chart:title-text logx-y "y = log(x)"))

          ; log-log plot of y = x**n
          (let* ((canvas (make-instance 'canvas :background :white))
                 (logx-logy (chart:create-logx-logy-plot canvas '(1 1000 "") '(1 1000000 ""))))
            (pack canvas :side :top)

            (chart:data-config logx-logy "series1" :colour :green)
            (chart:data-config logx-logy "series2" :colour :blue)
            (chart:data-config logx-logy "series3" :colour :red)

            (dolist (x '(1 2 5 10 20 50 100 200 500 1000))
              (chart:plot logx-logy "series1" x (sqrt x))
              (chart:plot logx-logy "series2" x (* x x))
              (chart:plot logx-logy "series3" x (* x (sqrt x))))

            (chart:title-text logx-logy "y = x**n, n= 1/2, 2, 3/2")
            (chart:x-ticklines logx-logy :grey)))

