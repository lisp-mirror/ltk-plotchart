; Translation of plotdemos19.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plotdemos19.lisp")
          (let* ((c1 (make-instance 'canvas :background :white :width 600 :height 300))
                 (c2 (make-instance 'canvas :background :white :width 600 :height 300))
                 (c3 (make-instance 'canvas :background :white :width 600 :height 300))
                 (s1 (chart:create-xy-plot c1 '(-10 10 5) '(-10 10 5) :axesatzero t))
                 (s2 (chart:create-xy-plot c2 '(-20 20 5) '(0 7 2) :isometric t))
                 (s3 (chart:create-histogram c3 '(0 10 "") '(0 10 5) :xlabels '(1 4 6))))
            (pack (list c1 c2 c3))

            (chart:title-text s1 "Axes at the origin (:axesatzero t)")
            (chart:plot s1 "data" 0 1)
            (chart:plot s1 "data" 1 2)
            (chart:plot s1 "data" 2 5)
            (chart:plot s1 "data" 6 2)

            (chart:title-text s2 "Squares appear as squares on the screen (:isometric t)")
            (chart:plot s2 "data" 4 7)
            (chart:plot s2 "data" 7 7)
            (chart:plot s2 "data" 7 4)
            (chart:plot s2 "data" 4 4)

            (chart:title-text s3 "Histogram with custom labels (:xlabels + xconfig)")
            (chart:x-config s3 :format "%.0fns")
            (chart:plot s3 "data" 0 1)
            (chart:plot s3 "data" 1 2)
            (chart:plot s3 "data" 2 5)
            (chart:plot s3 "data" 6 2)

          ))

