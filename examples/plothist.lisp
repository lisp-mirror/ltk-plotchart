;; Translation of plothist.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plothist.lisp")
          (let* ((c (make-instance 'canvas :width 600 :height 400 :background :white))
                 (h (chart:create-histogram c '(0.0 100.0 10.0)'(0.0 100.0 20.0))))

            (chart:data-config h "series1" :colour :green)

            (let ((xd 5.0)
                  (yd 20.0)
                  (xold 0.0)
                  (yold 50.0))
              (dotimes (i 20)
                (let ((xnew (+ xold xd))
                      (ynew (+ yold (* (- (random 1.0) 0.5) yd))))
                  (chart:plot h "series1" xnew ynew)
                  (setf xold xnew
                        yold ynew))))

            (chart:balloon-config h
                                  :background :green
                                  :rimwidth 3
                                  :arrowsize 10
                                  :font "Times 14")
            (chart:balloon h 50 50 "Here it is!" :south-east)

            (chart:balloon-config h :background :red)
            (chart:balloon-config h :margin 10)
            (chart:balloon h 50 100 "No, here!" :north-east)

            (chart:title-text h "Aha!")

            (pack c)))

