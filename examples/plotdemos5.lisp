; Translation of plotdemos5.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(defparameter *x* '((0.0 100.0 200.0) (0.0 100.0 200.0) (0.0 100.0 200.0) (0.0 100.0 200.0)))
(defparameter *y* '((0.0   0.0   0.0) (30.0  30.0  30.0) (60.0  60.0  60.0) (90.0  90.0  90.0)))
(defparameter *f* '((0.0   1.0  10.0) ( 0.0  30.0  30.0) (10.0  60.0  60.0) (30.0  90.0  90.0)))
(defparameter *contours* '(0.0             
                           5.2631578947    
                           10.5263157895   
                           15.7894736842   
                           21.0526315789   
                           26.3157894737   
                           31.5789473684   
                           36.8421052632   
                           42.1052631579   
                           47.3684210526   
                           52.6315789474   
                           57.8947368421   
                           63.1578947368   
                           68.4210526316   
                           73.6842105263   
                           78.9473684211   
                           84.2105263158   
                           89.4736842105   
                           94.7368421053   
                           100.0           
                           105.263157895))
(defparameter *xlimits* '(0 200 50))
(defparameter *ylimits* '(0 100 20))

(with-ltk ()
          (wm-title *tk* "Contour Demo : shade (jet colourmap)")
          (let* ((c (make-instance 'canvas :background :white :width 500 :height 500))
                 (chart (chart:create-xy-plot c *xlimits* *ylimits*)))
            (pack c :fill :both :side :top)
            (chart:colour-map :jet)
            (chart:draw-contour-fill chart *x* *y* *f* *contours*)
            (chart:draw-grid chart *x* *y*))

          (let* ((toplevel (make-toplevel *tk*))
                 (c (make-instance 'canvas :master toplevel
                                   :background :white :width 500 :height 500))
                 (chart (chart:create-xy-plot c *xlimits* *ylimits*)))
            (wm-title toplevel "Contour Demo : contourlines (default colourmap)")
            (pack c :fill :both :side :top)

            (chart:draw-contour-lines chart *x* *y* *f* *contours*)
            (chart:draw-grid chart *x* *y*))

          (let* ((toplevel (make-toplevel *tk*))
                 (c (make-instance 'canvas :master toplevel
                                   :background :white :width 500 :height 500))
                 (chart (chart:create-xy-plot c *xlimits* *ylimits*)))
            (wm-title toplevel "Contour Demo : contourlines (hot colourmap)")
            (pack c :fill :both :side :top)
            (chart:colour-map :hot)
            (chart:draw-contour-fill chart *x* *y* *f* *contours*)
            (chart:draw-grid chart *x* *y*))

          (let* ((toplevel (make-toplevel *tk*))
                 (c (make-instance 'canvas :master toplevel
                                   :background :white :width 500 :height 500))
                 (chart (chart:create-xy-plot c *xlimits* *ylimits*)))
            (wm-title toplevel "Contour Demo : grey contourfill, jet contourlines")
            (pack c :fill :both :side :top)

            (chart:colour-map :grey)
            (chart:draw-contour-fill chart *x* *y* *f* *contours*)
            (chart:colour-map :jet)
            (chart:draw-contour-lines chart *x* *y* *f* *contours*)
            (chart:draw-grid chart *x* *y*))


          (let* ((toplevel (make-toplevel *tk*))
                 (c (make-instance 'canvas :master toplevel
                                   :background :white :width 500 :height 500))
                 (chart (chart:create-xy-plot c *xlimits* *ylimits*)))
            (wm-title toplevel "Contour Demo : contourlines (cool colourmap)")
            (pack c :fill :both :side :top)
            (chart:colour-map :cool)
            (chart:draw-contour-fill chart *x* *y* *f* *contours*)
            (chart:draw-grid chart *x* *y*))
          
          (let* ((toplevel (make-toplevel *tk*))
                 (c (make-instance 'canvas :master toplevel
                                   :background :white :width 500 :height 500))
                 (chart (chart:create-xy-plot c *xlimits* *ylimits*)))
            (wm-title toplevel "Contour Demo : default contours (jet colourmap)")
            (pack c :fill :both :side :top)
            (chart:colour-map :jet)
            (chart:draw-contour-fill chart *x* *y* *f*)
            (chart:draw-grid chart *x* *y*))
          
          ; TODO chart 6 - plotfuncont not implemented
          )

