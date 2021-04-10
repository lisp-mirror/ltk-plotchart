;; Translation of plotdemos18.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plotdemos18.lisp")
          (let* ((devices '("e1" "e2" "e12231" "r1" "r2"))
                 (canvas (make-instance 'canvas :width 600 :height 400
                                        :background :white))
                 (timeline (chart:create-status-timeline canvas '(0 7200 900) devices :showxaxis nil)))
            (pack canvas :side :left :fill :both)

            ; add the randomised data 
            (do* ((li 0 i)
                  (i (random 10.0) (+ i (random 600.0))))
              ((>= i 7200))
              (dolist (item devices)
                (chart:plot timeline item li i (if (> (random 1.0) 0.5) :red :green))))

            ; add labelled vertical lines
            (loop for x from 0 to 7200 by 900
                  for text = (format nil "~2,'0Dh:~2,'0D" 
                                     (floor (/ x 3600)) 
                                     (floor (mod x 60)))
                  do (if (zerop (mod x 3600))
                       (chart:draw-vertical-line timeline text x :fill :black :width 2)
                       (chart:draw-vertical-line timeline "" x :fill :grey :dash "...")))

            (chart:title-text timeline "Operational over time")))

