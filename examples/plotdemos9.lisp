; Translation of plotdemos9.tcl

(require 'asdf)    
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plotdemos9.lisp")
          ; windrose diagram
          (let* ((canvas (make-instance 'canvas :background :white))
                 (p (chart:create-windrose canvas '(30 6) :num-sectors 4)))
            (pack canvas)

            (chart:plot p '(5 10 0 3) :red)
            (chart:plot p '(10 10 10 3) :blue)
            (chart:title-text p "Simple wind rose - margins need to be corrected ..."))

          ; bands in two directions
          (let* ((canvas (make-instance 'canvas :background :white))
                 (p (chart:create-xy-plot canvas '(0 10 2) '(0 40 10))))
            (pack canvas :side :top)

            (chart:plot p "data" 1 10)
            (chart:plot p "data" 6 20)
            (chart:plot p "data" 9 10)

            (chart:draw-x-band p 15 25)
            (chart:draw-y-band p 3 5))

          ; label-dots and vertical text
          (let* ((canvas (make-instance 'canvas :background :white))
                 (p (chart:create-xy-plot canvas '(0 10 2) '(0 40 10))))
            (pack canvas :side :top)
            
            (chart:draw-label-dot p 3 10 "Point 1" :w)
            (chart:draw-label-dot p 6 20 "Point 2" :e)
            (chart:draw-label-dot p 9 10 "Point 3" :n)
            (chart:draw-label-dot p 9 30 "Point 4" :s)

            (chart:v-text p "Vertical axis label"))
          )

