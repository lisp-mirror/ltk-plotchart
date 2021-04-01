;; Translation of plotdemos1.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)
(use-package :ltk-plotchart)

(setf *debug-tk* t)

(with-ltk ()
          ;; frame 1 - has four parts
          (wm-title *tk* "plotdemos1.lisp")
          (let ((c (make-instance 'canvas :background :white :width 400 :height 200))
                (c2 (make-instance 'canvas :background :white :width 400 :height 200))
                (c3 (make-instance 'canvas :background :white :width 400 :height 200))
                (c4 (make-instance 'canvas :background :white :width 400 :height 200)))
            (pack c :fill :both :side :top)
            (pack c2 :fill :both :side :top)
            (pack c3 :fill :both :side :top)
            (pack c4 :fill :both :side :top)

            ;; -- set up part 1
            (let ((s (make-instance 'xy-plot :canvas c :xaxis '(0.0 100.0 10.0) :yaxis '(0.0 100.0 20.0)))
                  (r (make-instance 'right-axis :canvas c :yaxis '(0.0 0.1 0.01)))
                  )
              (xy-plot-dataconfig s "series1" :colour :red)
              (xy-plot-dataconfig s "series2" :colour :blue)
              (xy-plot-dataconfig s "series3" :colour :magenta)

              (let ((xd 5.0) 
                    (yd 20.0) 
                    (xold 0.0) 
                    (yold 50.0))

                (dotimes (i 20)
                  (let ((xnew (+ xold xd))
                        (ynew (+ yold (* (- (random 1.0) 0.5) yd)))
                        (ynew2 (+ yold (* (- (random 1.0) 0.5) 2.0 yd))))
                    (xy-plot-plot s "series1" xnew ynew)
                    (xy-plot-plot s "series2" xnew ynew2)
                    (xy-plot-trend s "series3" xnew ynew)
                    (setf xold xnew
                          yold ynew))))

              (xy-plot-interval s "series2" 50.0 40.0 60.0 52.0)
              (xy-plot-interval s "series2" 60.0 40.0 60.0)

              (chart-xtext s "X-coordinate")
              (chart-ytext s "Y-data")
              (chart-ytext r "Right axis")
              (chart-title s "Aha!")

              ; some data for the right axis
              (right-axis-dataconfig r "right" :type :both :symbol :circle :colour :green)
              (right-axis-plot r "right" 10.0 0.01)
              (right-axis-plot r "right" 30.0 0.03)
              (right-axis-plot r "right" 40.0 0.02))

            ;; -- set up part 2
            (let ((s (make-instance 'pie-chart :canvas c2)))
              (pie-chart-plot s '(("Long names" . 10) ("Short names" . 30) ("Average" . 40)
                                                      ("Ultra-short names" . 5)))
              (chart-title s "Okay - this works"))

            ;; -- set up part 3
            (let ((s (make-instance 'polar-plot :canvas c3 :radius-data '(3.0 1.0))))
              (do ((angle 0 (+ angle 10.0)))
                ((>= angle 360))
                (let ((rad (+ 1.0 (cos (* pi (/ angle 180))))))
                  (polar-plot-plot s "cardioid" rad angle)))
              (chart-title s "Cardioid"))

            ;; -- set up part 4
            (let ((s (make-instance 'tx-plot 
                                    :canvas c4 
                                    :timeaxis '("2006-01-01" "2007-01-01" 120)
                                    :xaxis '(0.0 100.0 20.0))))
              (tx-plot-dataconfig s "series1" :colour :red)
              (tx-plot-dataconfig s "series2" :colour :blue)

              (chart-xtext s "Time")
              (chart-ytext s "Data")
              (chart-xticklines s)

              (tx-plot-plot s "series1" "2006-02-01" 10.0)
              (tx-plot-plot s "series1" "2006-02-11" 50.0)
              (tx-plot-plot s "series1" "2006-03-01" 50.0)
              (tx-plot-plot s "series1" "2006-07-01" 40.0)
              (tx-plot-plot s "series1" "2006-08-21" 20.0)
              (tx-plot-plot s "series1" "2006-08-22" 1.0)
              (tx-plot-plot s "series1" "2006-12-11" 78.0)

              (tx-plot-plot s "series2" "2006-03-01" 110.0)
              (tx-plot-plot s "series2" "2006-04-11" 50.0)
              (tx-plot-plot s "series2" "2006-07-28" 20.0)
              (tx-plot-plot s "series2" "2006-10-21" 99.0)
              (tx-plot-plot s "series2" "2006-11-22" 1.0)
              (tx-plot-plot s "series2" "2006-12-31" 78.0)))

          ;; frame 2 
          (let* ((toplevel-h (make-toplevel *tk*))
                (c (make-instance 'canvas :master toplevel-h :background :white :width 400 :height 200))
                (c2 (make-instance 'canvas :master toplevel-h :background :white :width 400 :height 200)))
            (wm-title toplevel-h "plotdemos1.lisp - h")

            (pack c :fill :both :side :top)
            (pack c2 :fill :both :side :top)

            (let ((s (make-instance 'bar-chart :canvas c :xlabels '(A B C D E) :yaxis '(0.0 10.0 2.0)
                                    :noseries 2.5)))
              (bar-chart-plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red)
              (bar-chart-plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green)

              (chart-title s "Arbitrary data")
              (chart-legend s "series1" "Series 1")
              (chart-legend s "series2" "Series 2" 20) ; extra spacing
              )

            (let ((s (make-instance 'bar-chart :canvas c2 
                                    :xlabels '(A B C D E)
                                    :yaxis '(0.0 20.0 5.0)
                                    :noseries :stacked)))
              (bar-chart-plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red)
              (bar-chart-plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green)
              (chart-title s "Stacked diagram")
              )
            )

          ;; frame 3
          (let* ((toplevel-v (make-toplevel *tk*))
                (c (make-instance 'canvas :master toplevel-v :background :white :width 400 :height 200))
                (c2 (make-instance 'canvas :master toplevel-v :background :white :width 400 :height 200))
                (c3 (make-instance 'canvas :master toplevel-v :background :white :width 400 :height 200)))
            (wm-title toplevel-v "plotdemos1.lisp - v")

            (pack c :fill :both :side :top)
            (pack c2 :fill :both :side :top)
            (pack c3 :fill :both :side :top)

            (let ((s (make-instance 'horizontal-bar-chart :canvas c 
                                    :xaxis '(0.0 10.0 2.0)
                                    :ylabels '(A B C D E) 
                                    :noseries 2)))
              (horizontal-bar-chart-plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red)
              (horizontal-bar-chart-plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green)

              (chart-title s "Arbitrary data")
              )

            (let ((s (make-instance 'horizontal-bar-chart :canvas c2 
                                    :xaxis '(0.0 20.0 5.0)
                                    :ylabels '(A B C D E)
                                    :noseries :stacked)))
              (horizontal-bar-chart-plot s "series1" '(1.0 4.0 6.0 1.0 7.0) :red)
              (horizontal-bar-chart-plot s "series2" '(0.0 3.0 7.0 9.3 2.0) :green)
              (chart-title s "Stacked diagram")
              )

            (let ((s (make-instance 'time-chart :canvas c3
                                    :time-begin "1 january 2004"
                                    :time-end "31 december 2004"
                                    :num-items 4)))
              (time-chart-period s "Spring" "1 march 2004" "1 june 2004" :green)
              (time-chart-period s "Summer" "1 june 2004" "1 september 2004" :yellow)
              (time-chart-vertline s "1 jan" "1 january 2004")
              (time-chart-vertline s "1 apr" "1 april 2004")
              (time-chart-vertline s "1 jul" "1 july 2004")
             (time-chart-vertline s "1 oct" "1 october 2004")
              (time-chart-milestone s "Longest day" "21 july 2004")
              (chart-title s "Seasons (northern hemisphere)")
              )
            )

          ;; frame 4
          (let* ((toplevel-h3 (make-toplevel *tk*))
                (c (make-instance 'canvas :master toplevel-h3 :background :white :width 400 :height 300))
                (c2 (make-instance 'canvas :master toplevel-h3 :background :white :width 400 :height 250))
                (c3 (make-instance 'canvas :master toplevel-h3 :background :white :width 400 :height 250)))
            (wm-title toplevel-h3 "plotdemos1.lisp - h3")

            (pack c :fill :both :side :top)
            (pack c2 :fill :both :side :top)
            (pack c3 :fill :both :side :top)

            ;; cowboy hat - in TclTk this is provided as a function, but that is not supported
            (labels ((square (n) (* n n))
                     (cowboyhat (x y)
                                (let ((x1 (/ x 9.0))
                                      (y1 (/ y 9.0)))
                                  (* 3.0 
                                     (- 1.0 (+ (square x1) (square y1)))
                                     (- 1.0 (+ (square x1) (square y1)))))))
              (let ((s (make-instance 'threed-plot :canvas c
                                      :xaxis '(0 10 3) 
                                      :yaxis '(-10 10 10)
                                      :zaxis '(0 10 2.5))))
                (chart-title s "3D Plot")
                (threed-plot-plotdata s (mapcar #'(lambda (r) 
                                                    (mapcar #'(lambda (c)
                                                                (cowboyhat r c))
                                                            '(0 1 2 3 4 5 6 7 8 9 10)))
                                                '(-10 -8 -6 -4 -2 0 2 4 6 8 10)))))

            (let ((s (make-instance 'threed-plot :canvas c2 
                                    :xaxis '(0 10 3)
                                    :yaxis '(-10 10 10)
                                    :zaxis '(0 10 2.5)
                                    :xlabels '(A B C))))
              (chart-title s "3D Plot - data")
              (threed-plot-colour s :green :black)
              (threed-plot-plotdata s '((1.0 2.0 1.0 0.0) (1.1 3.0 1.1 -0.5) (3.0 1.0 4.0 5.0))))

            (let ((s (make-instance 'threed-plot :canvas c3 
                                    :xaxis '(0 10 3)
                                    :yaxis '(-10 10 10)
                                    :zaxis '(0 10 2.5))))
              (chart-title s "3D Plot - data")
              (threed-plot-colour s :green :black)
              (threed-plot-interpolatedata s 
                                           '((1.0 2.0 1.0 0.0) (1.1 3.0 1.1 -0.5) (3.0 1.0 4.0 5.0))
                                           '(0.0 0.5 1.0 1.5 2.0)))

            )
          )

