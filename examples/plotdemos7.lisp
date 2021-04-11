; Translation of plotdemos7.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          ;; frame 1
          (wm-title *tk* "plotdemos7.lisp")
          (let ((c (make-instance 'canvas :background :white :width 400 :height 200))
                (c2 (make-instance 'canvas :background :white :width 400 :height 200))
                (c3 (make-instance 'canvas :background :white :width 400 :height 200)))
            (pack (list c c2 c3) :fill :both :side :top)

            ;; customise the xy-plot
            (chart:plot-config :xyplot :title :font "Times 14")
            (chart:plot-config :xyplot :title :textcolor :red)
            (chart:plot-config :xyplot :leftaxis :font "Helvetical 10 italic")
            (chart:plot-config :xyplot :leftaxis :thickness 2)
            (chart:plot-config :xyplot :leftaxis :ticklength -5)
            (chart:plot-config :xyplot :rightaxis :font "Times 10 bold")
            (chart:plot-config :xyplot :rightaxis :color :green)
            (chart:plot-config :xyplot :rightaxis :thickness 2)
            (chart:plot-config :xyplot :margin :right 100)

            ;; -- set up part 1
            (let ((s (chart:create-xy-plot c '(0.0 100.0 10.0) '(0.0 100.0 20.0)))
                  (r (chart:create-right-axis c '(0.0 0.1 0.01))))
              (chart:data-config s "series1" :colour :red)
              (chart:data-config s "series2" :colour :blue)
              (chart:data-config s "series3" :colour :magenta)

              (let ((xd 5.0) 
                    (yd 20.0) 
                    (xold 0.0) 
                    (yold 50.0))

                (dotimes (i 20)
                  (let ((xnew (+ xold xd))
                        (ynew (+ yold (* (- (random 1.0) 0.5) yd)))
                        (ynew2 (+ yold (* (- (random 1.0) 0.5) 2.0 yd))))
                    (chart:plot s "series1" xnew ynew)
                    (chart:plot s "series2" xnew ynew2)
                    (chart:draw-trendline s "series3" xnew ynew)
                    (setf xold xnew
                          yold ynew))))

              (chart:draw-interval s "series2" 50.0 40.0 60.0 52.0)
              (chart:draw-interval s "series2" 60.0 40.0 60.0)

              (chart:x-text s "X-coordinate")
              (chart:y-text s "Y-data")
              (chart:y-text r "Right axis")
              (chart:title-text s "Aha!")

              ; some data for the right axis
              (chart:data-config r "right" :type :both :symbol :circle :colour :green)
              (chart:plot r "right" 10.0 0.01)
              (chart:plot r "right" 30.0 0.03)
              (chart:plot r "right" 40.0 0.02))

            ;; -- set up part 2
            (let ((s (chart:create-pie-chart c2)))
              (chart:plot s '(("Long names" 10) ("Short names" 30) ("Average" 40)
                                                  ("Ultra-short names" 5)))
              (chart:title-text s "Okay - this works"))

            ;; -- set up part 3
            (let ((s (chart:create-polar-plot c3 '(3.0 1.0))))
              (do ((angle 0 (+ angle 10.0)))
                ((>= angle 360))
                (let ((rad (+ 1.0 (cos (* pi (/ angle 180))))))
                  (chart:plot s "cardioid" rad angle)))
              (chart:title-text s "Cardioid"))

            )

          ;; frame 2 
          (let* ((toplevel-h (make-toplevel *tk*))
                 (c (make-instance 'canvas :master toplevel-h :background :white :width 400 :height 200))
                 (c2 (make-instance 'canvas :master toplevel-h :background :white :width 400 :height 200)))
            (wm-title toplevel-h "plotdemos1.lisp - h")

            (pack (list c c2) :fill :both :side :top)

            (let ((s (chart:create-bar-chart c '(A B C D E) '(0.0 10.0 2.0) 2.5)))
              (chart:plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red)
              (chart:plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green)
              (chart:title-text s "Arbitrary data")

              (chart:legend s "series1" "Series 1")
              (chart:legend s "series2" "Series 2") 
              )

            (let ((s (chart:create-bar-chart c2 
                                             '(A B C D E)
                                             '(0.0 20.0 5.0)
                                             :stacked)))
              (chart:plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red)
              (chart:plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green)
              (chart:title-text s "Stacked diagram")))

          ;; frame 3
          (let* ((toplevel-v (make-toplevel *tk*))
                 (c (make-instance 'canvas :master toplevel-v :background :white :width 400 :height 200))
                 (c2 (make-instance 'canvas :master toplevel-v :background :white :width 400 :height 200))
                 (c3 (make-instance 'canvas :master toplevel-v :background :white :width 400 :height 200)))
            (wm-title toplevel-v "plotdemos1.lisp - v")

            (pack (list c c2 c3) :fill :both :side :top)

            (chart:plot-config :horizbars :leftaxis :font "Helvetica 10 italic")
            (chart:plot-config :horizbars :background :outercolor :steelblue3)
            (chart:plot-config :horizbars :bottomaxis :ticklength -5)

            (let ((s (chart:create-horizontal-bar-chart c 
                                                        '(0.0 10.0 2.0)
                                                        '("Antarctica" "Eurasia" "The Americas" "Australia and Oceania" "Ocean") 
                                                        2)))

              (chart:plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red :left-right)
              (chart:plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green :right-left)

              (chart:title-text s "Arbitrary data"))

            (let ((s (chart:create-horizontal-bar-chart c2 
                                                        '(0.0 20.0 5.0)
                                                        '(A B C D E)
                                                        :stacked)))
              (chart:plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red :left-right)
              (chart:plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green)
              (chart:title-text s "Stacked diagram"))

            (let ((s (chart:create-time-chart c3
                                              "1 january 2004"
                                              "31 december 2004"
                                              :num-items 4)))
              (chart:period s "Spring" "1 march 2004" "1 june 2004" :green)
              (chart:period s "Summer" "1 june 2004" "1 september 2004" :yellow)
              (chart:add-period s "21 september 2004" "21 october 2004" :blue)
              (chart:draw-vertical-line s "1 jan" "1 january 2004")
              (chart:draw-vertical-line s "1 apr" "1 april 2004")
              (chart:draw-vertical-line s "1 jul" "1 july 2004")
              (chart:draw-vertical-line s "1 oct" "1 october 2004")
              (chart:milestone s "Longest day" "21 july 2004")
              (chart:add-milestone s "21 december 2004")
              (chart:title-text s "Seasons (northern hemisphere)")))
          )

