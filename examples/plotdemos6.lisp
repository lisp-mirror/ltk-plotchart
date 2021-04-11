; Translation of plotdemos6.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(defun hypot (x y) 
  (sqrt (+ (* x x) (* y y))))

(defun forces-dipole (x y)
  (let* ((xd1 51.0)
         (yd1 50.0)
         (xd2 49.0)
         (yd2 50.0)
         (r1p3 (expt (hypot (- x xd1) (- y yd1)) 3.0))
         (r2p3 (expt (hypot (- x xd2) (- y yd2)) 3.0))
         (fx (- (/ (- x xd1) r1p3) (/ (- x xd2) r2p3)))
         (fy (- (/ (- y yd1) r1p3) (/ (- y yd2) r2p3))))
    (list fx fy)))

(with-ltk ()
          (wm-title *tk* "plotdemos6.lisp")

          (let* ((c (make-instance 'canvas :background :white :width 400 :height 400))
                 (c2 (make-instance 'canvas :background :white :width 400 :height 200)))
            (pack (list c c2) :fill :both :side :top)

            (let ((s (chart:create-xy-plot c '(0.0 100.0 10.0) '(0.0 100.0 20.0))))
              (chart:vector-config s "series1" :colour :red :scale 40)
              (chart:vector-config s "series2" :colour :blue :scale 50 :type :nautical :centred 1)

              ; Cartesian
              (dolist (pair '((1.0 0.0) (0.0 1.0) (0.5 0.5) (-2.0 1.0)))
                (chart:draw-vector s "series1" 30.0 20.0 (first pair) (second pair)))
              ; Nautical
              (dolist (pair '((1.0 0.0) (1.0 45.0) (2.0 90.0)))
                (chart:draw-vector s "series2" 60.0 40.0 (first pair) (second pair))))

            (let ((s (chart:create-xy-plot c2 '(0.0 100.0 10.0) '(0.0 100.0 20.0))))
              (chart:dot-config s "series1" :colour :red :scalebyvalue t :scale 2.5)
              (chart:dot-config s "series2" :colour :magenta 
                                :classes '(0 :blue 1 :green 2 :yellow 3 :red)
                                :scalebyvalue nil :outline 0)
              (chart:dot-config s "series3" :colour :magenta
                                :classes '(0 :blue 1 :green 2 :yellow 3 :red)
                                :scalebyvalue t :outline 2.5)
              (let ((y1 20) (y2 50) (y3 80) (x 10))
                (dolist (value '(-1.0 0.5 1.5 2.5 3.5 4.5))
                  (chart:draw-dot s "series1" x y1 value)
                  (chart:draw-dot s "series2" x y2 value)
                  (chart:draw-dot s "series3" x y3 value)
                  (incf x 10)))))

          ; dipole example
          (let* ((dipole (make-toplevel *tk*))
                 (canvas (make-instance 'canvas :master dipole
                                        :background :white :width 500 :height 500))
                 (s (chart:create-xy-plot canvas '(45.0 55.0 1.0) '(45.0 55.0 1.0))))
            (wm-title dipole "plotdemos6.lisp - dipole")
            (pack canvas :fill :both :side :top)

            (chart:title-text s "Forces in a dipole field")
            (chart:vector-config s "series1" :colour :black :scale 40 :type :polar)
            (chart:dot-config s "dipole" :colour :red :scalebyvalue nil :radius 5)
            (chart:draw-dot s "dipole" 49.0 50.0 1.0)
            (chart:draw-dot s "dipole" 51.0 50.0 1.0)

            (loop for y from 45.25 to 55.0 by 0.5
                  do (loop for x from 45.25 to 55.0 by 0.5
                           for uv = (forces-dipole x y)
                           ; scale vector for better display
                           do (chart:draw-vector s "series1" 
                                                 x y
                                                 (/ (+ 0.5 (hypot (first uv) (second uv)))
                                                    (+ 1.0 (hypot (first uv) (second uv))))
                                                 (/ (* 180.0 (atan (second uv) (first uv))) pi)))))

          ; rchart demonstration
          (let* ((rchart (make-toplevel *tk*))
                 (canvas (make-instance 'canvas :master rchart
                                        :background :white :width 400 :height 200))
                 (s (chart:create-xy-plot canvas '(0 100.0 10.0) '(0.0 50.0 10.0))))
            (pack canvas :fill :both :side :top)

            (wm-title rchart "plotdemos6.lisp - rchart")
            (chart:title-text s "R-chart (arbitrary data)")

            (chart:data-config s "series1" :colour :green)
            (loop for x from 1.0 to 50.0 by 3.0
                  for y = (+ 20.0 (random 3.0))
                  do (chart:rchart s "series1" x y))

            ; now some data outside the expected range
            (chart:rchart s "series1" 50.0 41.0)
            (chart:rchart s "series1" 52.0 42.0)
            (chart:rchart s "series1" 54.0 39.0)

            ; and continue with the well-behaved series
            (loop for x from 57.0 to 100.0 by 3.0
                  for y = (+ 20.0 (random 3.0))
                  do (chart:rchart s "series1" x y)))

          )

