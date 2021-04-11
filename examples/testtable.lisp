; Translation of testtable.tcl

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "testtable.lisp")

          (let ((c (make-instance 'canvas :background :white :height 300))
                (c2 (make-instance 'canvas :background :white :height 300)))
            (pack (list c c2) :side :left :fill :both :expand :yes)

            (chart:plot-config "table" :frame :outerwidth 3)
            (chart:plot-config "table" :frame :color :red)
            (chart:plot-config "table" :subtitle :font "Courier 14")

            (let ((t1 (chart:create-table-chart c '("Column 1" "Column 2" "Column 3") 
                                                :widths 80)))
              ; TODO? formatcommand

              (chart:title-text t1 "Demonstration of table charts")
              (chart:separator t1)

              (dotimes (i 9)
                (when (= i 3) (chart:separator t1))
                (chart:add-row t1 (list (random 10.0) (random 10.0) (random 10.0)))))

            ; second type of table
            (chart:plot-config "table" :frame :color :none)
            (let ((t2 (chart:create-table-chart c2 '("Company" "Change (%)" "Current Price")
                                                :widths '(80 120 30))))
              ; TODO? formatcommand

              (chart:add-row t2 '("Company A" -3.0 16.0))
              (chart:add-row t2 '("Company B" 1.8 224.2))
              (chart:add-row t2 '("Company C" 0.8 10.0))
              (chart:add-row t2 '("Company D" -6.8 45.3)))

            ))

