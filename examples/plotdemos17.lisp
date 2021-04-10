; Translation of plotdemos17.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plotdemos17.lisp")
          (let ((c1 (make-instance 'canvas :background :white :width 400 :height 300))
                (c2 (make-instance 'canvas :background :white :width 400 :height 300))
                (c3 (make-instance 'canvas :background :white :width 400 :height 200)))
            (pack (list c1 c2 c3))

            (let ((p1 (chart:create-xy-plot c1 '(0 10 2) '(-1 1 0.25)))
                  (p2 (chart:create-xy-plot c2 '(0 10 2) '(-10 10 5))))
              (chart:y-text p1 "(V)")
              (chart:x-config p1 :tick-length 20 :format "%.3f")
              (chart:y-config p1 :tick-length 20 :minor-ticks 4 :label-offset 10)
              
              (chart:y-text p2 "(mA)")
              (chart:data-config p2 "current" :colour :red)
              (chart:x-config p2 :tick-length 6 :minor-ticks 1)

              ; fill in some data
              (loop for i from 0 to 100 
                    for phase = (/ (* 2.0 PI i) 50.0)
                    do (progn (chart:plot p1 "voltage" phase (* 0.9 (cos phase)))
                              (chart:plot p2 "current" phase (* 7 (sin phase))))))

            ; third plot uses xlabels and ylabels
            (chart:create-xy-plot c3 '(0 10 "") '(-10 10 "") :xlabels '(1 4 6) :ylabels '(-5 0))))

