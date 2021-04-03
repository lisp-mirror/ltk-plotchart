;; Translation of plotdemos3.tcl
;; -- TODO: chart:task for gantt must return descriptor for use in summary/connect

(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "plotdemos3.lisp")
          (let ((c (make-instance 'canvas :width 500 :height 200 :background :white)))
            (pack c :fill :both)
            (let* ((gantt (chart:create-gantt-chart c "1 january 2004" "31 december 2004" :num-items 4))
                   (from (chart:task gantt "Spring" "1 march 2004" "1 june 2004" 30))
                   (to (chart:task gantt "Summer" "1 june 2004" "1 september 2004" 10)))
;              (chart:summary gantt "First half" from to)
;              (chart:connect gantt from to)
              (chart:vertical-line gantt "1 jan" "1 january 2004")
              (chart:vertical-line gantt "1 apr" "1 april 2004")
              (chart:vertical-line gantt "1 jul" "1 july 2004")
              (chart:vertical-line gantt "1 oct" "1 october 2004")
              (chart:milestone gantt "Longest day" "21 july 2004")
              (chart:title-text gantt "Seasons (northern hemisphere)")

              ;; make copies
              (let* ((window (make-instance 'toplevel 
                                            :master *tk* 
                                            :title "plotdemos3.lisp - .t.c"))
                     (canvas (make-instance 'canvas :master window :width 700 :height 500)))
                (pack canvas)
                (chart:plot-pack canvas :top gantt gantt)
                (chart:plot-pack canvas :left gantt)))))

