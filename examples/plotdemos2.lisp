;; Translation of plotdemos2.tcl
;; - TODO: 'after' is not working with the stripchart variable
;;   so gendata is not visibly incremental

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(defun gendata (stripchart xold xd yold yd)
  (let ((xnew (+ xold xd))
        (ynew (+ yold (* (- (random 1.0) 0.5) yd)))
        (ynew2 (+ yold (* (- (random 1.0) 0.5) 2.0 yd))))
    (chart:plot stripchart "series1" xnew ynew)
    (chart:plot stripchart "series2" xnew ynew2)

    (when (< xnew 200)
      (gendata stripchart xnew xd ynew yd))))

(with-ltk ()
          ;; frame 1 - has two parts
          (wm-title *tk* "plotdemos2.lisp")
          (let ((c (make-instance 'canvas :background :white :width 400 :height 200))
                (c2 (make-instance 'canvas :background :white :width 400 :height 200)))

            (pack (list c c2) :fill :both :side :top)

            ;; -- set up a strip chart
            (let ((s (chart:create-strip-chart c '(0.0 100.0 10.0) '(0.0 100.0 20.0))))

              (chart:title-text s "Aha!")
              (gendata s 0.0 15.0 50.0 30.0))

            ;; -- set up an isometric plot
            (let ((s (chart:create-isometric-plot c2 '(0.0 100.0) '(0.0 200.0) :noaxes)))
              ;; TODO setZoomPan ?
              (chart:draw-rectangle s 10.0 10.0 50.0 50.0 :green)
              (chart:draw-filled-rectangle s 20.0 20.0 40.0 40.0 :red)
              (chart:draw-filled-circle s 70.0 70.0 40.0 :yellow)
              (chart:draw-circle s 70.0 70.0 42.0)))

          ;; frame 2 - check the symbols
          (let* ((window (make-instance 'toplevel
                                        :title "plotdemos2.lisp - h"))
                 (c (make-instance 'canvas :master window 
                                   :background :white :width 400 :height 200)))
            (pack c :fill :both)

            (let ((s (chart:create-xy-plot c '(0.0 100.0 10.0) '(0.0 100.0 20.0))))
              (chart:data-config s "series1" :colour :red :type :symbol)
              (chart:data-config s "series2" :colour :green :type :both)
              (chart:y-config s :format "%12.2e")

              (let ((x 5.0))
                (dolist (symbol '(plus cross circle up down dot upfilled downfilled))
                  (chart:data-config s "series1" :symbol symbol)
                  (chart:data-config s "series2" :symbol symbol)
                  (chart:plot s "series1" x 50.0)
                  (chart:plot s "series2" x 20)
                  (setf x (+ x 10.0))))))

          ;; frame 3 - xy-plot with background and a pareto plot

          (let* ((window (make-instance 'toplevel
                                        :title "plotdemos2.lisp - t2"))
                 (c (make-instance 'canvas :master window
                                   :background :white :width 400 :height 200))
                 (c2 (make-instance 'canvas :master window
                                    :background :white :width 400 :height 200)))
            (pack (list c c2) :fill :both)

            (let ((s (chart:create-xy-plot c '(0.0 100.0 10.0) '(0.0 100.0 20.0))))
              (chart:background s :gradient :green :top-down)
              (chart:data-config s "series1" :filled :up :fillcolour :white)

              (chart:plot s "series1" 0.0 20.0)
              (chart:plot s "series1" 10.0 20.0)
              (chart:plot s "series1" 30.0 50.0)
              (chart:plot s "series1" 35.0 45.0)
              (chart:plot s "series1" 45.0 25.0)
              (chart:plot s "series1" 75.0 55.0)
              (chart:plot s "series1" 100.0 55.0)

              (chart:plaintext s 30.0 60.0 "Peak" :south))

            (let ((s (chart:create-xy-plot c2 '(0.0 100.0 10.0) '(0.0 100.0 20.0)))
                  (image (image-load (make-image) "examples/tcllogo.gif")))
              (chart:background s :image image)
              (chart:plot s "series1" 0.0 20.0)
              (chart:plot s "series1" 10.0 20.0)
              (chart:plot s "series1" 30.0 50.0)
              (chart:plot s "series1" 35.0 45.0)
              (chart:plot s "series1" 45.0 25.0)
              (chart:plot s "series1" 75.0 55.0)
              (chart:plot s "series1" 100.0 55.0)))

          )
