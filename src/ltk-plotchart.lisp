;; LTk wrapper around the tklib plotchart module
;;
;; -- definition and naming follows that in the plotchart documentation
;; https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/plotchart/plotchart.html
;;
;; Not implemented (because ...):
;;
;; - chart-saveplot - option for :format requires Img library, so not supported
;; - $anyplot object itemtype series args
;; - $anyplot deletedata
;; - normal-plot - requires math::statistics package
;; - 3d-ribbon-chart - example fails in TCL code
;; - 3dplots - plotfunc, plotfuncont - not sure how to handle function (callback?)
;; - $table formatcommand procname - not sure how to handle procedures
;; - no options to plot-pack - &key and &rest ? 

(in-package :ltk-plotchart)

(eval-when (:load-toplevel)
  (setf *init-wish-hook* (append *init-wish-hook*
                                 (list (lambda ()
                                   (send-wish "package require Plotchart"))))))

;; ---------------------------------------------------------------------------
;; Classes for all supported plotchart types

;; Plotchart - parent class and common functions

(defvar *plot-count* 0)

(defun new-name ()
  (incf *plot-count*)
  (format nil "plotchart_~d" *plot-count*))

(defclass plotchart ()
  ((name :accessor name :initform "plotchart")
   (canvas :accessor canvas :initarg :canvas :initform nil)))

(defmethod initialize-instance :before ((chart plotchart) &key)
  (setf (name chart) (new-name)))

(defclass xy-plot-group (plotchart) ()) ; used to group methods, no initialisation

;; Bar Chart

(defclass bar-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart bar-chart) &key xlabels yaxis num-series xlabel-angle)
  (when (valid-axis-p yaxis "bar-chart - yaxis")
    (format-wish "set ~a [::Plotchart::createBarchart ~a ~a ~a ~a~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xlabels)
                 (tcl-it yaxis)
                 (tcl-it num-series)
                 (if xlabel-angle (format nil " -xlabelangle ~d" xlabel-angle) ""))))

(defun create-bar-chart (canvas xlabels yaxis num-series &key xlabel-angle)
  "Creates instance of a bar-chart"
  (make-instance 'bar-chart
                 :canvas canvas
                 :xlabels xlabels
                 :yaxis yaxis
                 :num-series num-series
                 :xlabel-angle xlabel-angle))

;; Box Plot

(defclass box-plot (plotchart)
  ())

(defmethod initialize-instance :after ((chart box-plot) &key xdata ydata orientation)
  (format-wish "set ~a [::Plotchart::createBoxplot ~a ~a ~a ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (tcl-it xdata)
               (tcl-it ydata)
               (if orientation
                 (tcl-it-checked orientation '("horizontal" "vertical") "orientation")
                 "")))

(defun create-box-plot (canvas xdata ydata &key (orientation :horizontal))
  "Creates an instance of a box-plot"
  (make-instance 'box-plot
                 :canvas canvas
                 :xdata xdata
                 :ydata ydata
                 :orientation orientation))

;; Gantt Chart

(defclass gantt-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart gantt-chart) &key time-begin time-end num-items max-width bar-height ylabel-width)
  (format-wish "set ~a [::Plotchart::createGanttchart ~a \"~a\" \"~a\" ~a ~a ~a ~a]"
               (name chart)
               (widget-path (canvas chart))
               time-begin 
               time-end 
               (if num-items num-items (if max-width "1" "")) ; note, max-width must be second
               (if max-width max-width "")
               (if bar-height (format nil "-barheight ~d" bar-height) "")
               (if ylabel-width (format nil "-ylabelwidth ~d" ylabel-width) "")))

(defun create-gantt-chart (canvas time-begin time-end &key num-items max-width bar-height ylabel-width)
  "Creates an instance of a gantt-chart"
  (make-instance 'gantt-chart
                 :canvas canvas
                 :time-begin time-begin
                 :time-end time-end
                 :num-items num-items
                 :max-width max-width
                 :bar-height bar-height
                 :ylabel-width ylabel-width))

;; Histogram 

(defclass histogram (plotchart)
  ())

(defmethod initialize-instance :after ((chart histogram) &key xaxis yaxis
                                                         xlabels ylabels box
                                                         axesbox timeformat gmt)
  (when (and (valid-axis-p xaxis "histogram - xaxis")
             (valid-axis-p yaxis "histogram - yaxis"))
    (format-wish "set ~a [::Plotchart::createHistogram ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis)
                 (tcl-it yaxis)
                 (make-xy-args xlabels ylabels box axesbox timeformat gmt))))

(defun create-histogram (canvas xaxis yaxis &key xlabels ylabels box
                                axesbox timeformat gmt)
  "Creates instance of an histogram"
  (make-instance 'histogram
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :xlabels xlabels
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt))

;; Horizontal Bar Chart

(defclass horizontal-bar-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart horizontal-bar-chart) &key xaxis ylabels num-series)
  (when (valid-axis-p xaxis "horizontal-bar-chart - xaxis")
    (format-wish "set ~a [::Plotchart::createHorizontalBarchart ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis) 
                 (tcl-it ylabels) 
                 (tcl-it num-series))))

(defun create-horizontal-bar-chart (canvas xaxis ylabels num-series)
  "Creates instance of a horizontal-bar-chart"
  (make-instance 'horizontal-bar-chart
                 :canvas canvas
                 :xaxis xaxis
                 :ylabels ylabels
                 :num-series num-series))

;; Isometric Plot

(defclass isometric-plot (plotchart)
  ())

(defmethod initialize-instance :after ((chart isometric-plot) &key xaxis yaxis stepsize)
  (when (and (valid-number-pair-p xaxis "isometric-plot - xaxis")
             (valid-number-pair-p yaxis "isometric-plot - yaxis"))
    (format-wish "set ~a [::Plotchart::createIsometricPlot ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis) 
                 (tcl-it yaxis)
                 (tcl-it stepsize))))

(defun create-isometric-plot (canvas xaxis yaxis stepsize)
  "Creates instance of an isometric-plot"
  (make-instance 'isometric-plot
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :stepsize stepsize))

;; LogX-LogY Plot
;; - XY Plot with both axes having logarithmic values

(defclass logx-logy-plot (xy-plot-group)
  ())

(defmethod initialize-instance :after ((chart logx-logy-plot) &key 
                                                              xaxis yaxis
                                                              xlabels ylabels box
                                                              axesbox timeformat gmt
                                                              axesatzero isometric)
  (when (and (valid-axis-p xaxis "logx-logy-plot - xaxis")
             (valid-axis-p yaxis "logx-logy-plot - yaxis"))
    (format-wish "set ~a [::Plotchart::createLogXLogYPlot ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis)
                 (tcl-it yaxis)
                 (make-xy-args xlabels ylabels box axesbox timeformat gmt axesatzero isometric))))

(defun create-logx-logy-plot (canvas xaxis yaxis &key xlabels ylabels box
                                     axesbox timeformat gmt
                                     axesatzero isometric)
  "Creates instance of a logx-logy-plot"
  (make-instance 'logx-logy-plot
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :xlabels xlabels
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt
                 :axesatzero axesatzero
                 :isometric isometric))

;; LogX-Y Plot
;; - XY Plot with x axis having logarithmic values

(defclass logx-y-plot (xy-plot-group)
  ())

(defmethod initialize-instance :after ((chart logx-y-plot) &key 
                                                           xaxis yaxis
                                                           xlabels ylabels box
                                                           axesbox timeformat gmt
                                                           axesatzero isometric)
  (when (and (valid-axis-p xaxis "logx-y-plot - xaxis")
             (valid-axis-p yaxis "logx-y-plot - yaxis"))
    (format-wish "set ~a [::Plotchart::createLogXYPlot ~a ~a ~a~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis)
                 (tcl-it yaxis) 
                 (make-xy-args xlabels ylabels box axesbox timeformat gmt axesatzero isometric))))

(defun create-logx-y-plot (canvas xaxis yaxis &key xlabels ylabels box
                                  axesbox timeformat gmt
                                  axesatzero isometric)
  "Creates instance of a logx-y-plot"
  (make-instance 'logx-y-plot
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :xlabels xlabels
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt
                 :axesatzero axesatzero
                 :isometric isometric))

;; Pie Chart

(defclass pie-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart pie-chart) &key)
  (format-wish "set ~a [::Plotchart::createPiechart ~a]" (name chart) (widget-path (canvas chart))))

(defun create-pie-chart (canvas)
  "Creates instance of a pie-chart"
  (make-instance 'pie-chart :canvas canvas))

;; Polar Plot

(defclass polar-plot (plotchart)
  ())

(defmethod initialize-instance :after ((chart polar-plot) &key radius-data
                                                          xlabels ylabels box
                                                          axesbox timeformat gmt
                                                          axesatzero isometric)
  (when (valid-number-pair-p radius-data "polar-plot - radius-data")
    (format-wish "set ~a [::Plotchart::createPolarplot ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it radius-data)
                 (make-xy-args xlabels ylabels box axesbox timeformat gmt axesatzero isometric))))

(defun create-polar-plot (canvas radius-data &key xlabels ylabels box axesbox
                                 timeformat gmt axesatzero isometric)
  "Creates instance of polar-plot"
  (make-instance 'polar-plot
                 :canvas canvas
                 :radius-data radius-data
                 :xlabels xlabels
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt
                 :axesatzero axesatzero
                 :isometric isometric))

;; Radial Chart

(defclass radial-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart radial-chart) &key names scale style)
  (format-wish "set ~a [::Plotchart::createRadialchart ~a ~a ~f ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (tcl-it names)
               scale 
               (if style
                 (tcl-it-checked style '("lines" "cumulative" "filled") "radial-chart - style")
                 "")))

(defun create-radial-chart (canvas names scale &key (style "lines"))
  "Creates an instance of a radial-chart"
  (make-instance 'radial-chart
                 :canvas canvas
                 :names names
                 :scale scale
                 :style style))

;; Right Axis

(defclass right-axis (plotchart)
  ())

(defmethod initialize-instance :after ((chart right-axis) &key yaxis)
  (when (valid-axis-p yaxis "right-axis - yaxis")
    (format-wish "set ~a [::Plotchart::createRightAxis ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it yaxis))))

(defun create-right-axis (canvas yaxis)
  "Creates an instance of a right-axis"
  (make-instance 'right-axis :canvas canvas :yaxis yaxis))

;; Spiral Pie 

(defclass spiral-pie (plotchart) 
  ())

(defmethod initialize-instance :after ((chart spiral-pie) &key)
  (format-wish "set ~a [::Plotchart::createSpiralPie ~a]" (name chart) (widget-path (canvas chart))))

(defun create-spiral-pie (canvas)
  "Creates an instance of a spiral-pie"
  (make-instance 'spiral-pie :canvas canvas))

;; Status timeline

(defclass status-timeline (plotchart)
  ())

(defmethod initialize-instance :after ((chart status-timeline) &key xaxis ylabels box axesbox showxaxis)
  (when (valid-axis-p xaxis "status-timeline - xaxis")
    (format-wish "set ~a [::Plotchart::createStatusTimeline ~a ~a ~a ~a]"
                 (name chart)
                 (widget-path (canvas chart))
                 (tcl-it xaxis)
                 (tcl-it ylabels)
                 (make-args (list (list "-box" box)
                                  (list "-axesbox" axesbox)
                                  (list "-xaxis" (tcl-it-boolean showxaxis)))))))

(defun create-status-timeline (canvas xaxis ylabels &key box axesbox showxaxis)
  "Creates an instance of status-timeline"
  (make-instance 'status-timeline
                 :canvas canvas
                 :xaxis xaxis
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :showxaxis showxaxis))

;; Strip Chart

(defclass strip-chart (xy-plot-group)
  ())

(defmethod initialize-instance :after ((chart strip-chart) &key xaxis yaxis 
                                                           xlabels ylabels box
                                                           axesbox timeformat gmt
                                                           axesatzero isometric)
  (when (and (valid-axis-p xaxis "strip-chart - xaxis")
             (valid-axis-p yaxis "strip-chart - yaxis"))
    (format-wish "set ~a [::Plotchart::createStripchart ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis)
                 (tcl-it yaxis)
                 (make-xy-args xlabels ylabels box axesbox timeformat gmt axesatzero isometric))))

(defun create-strip-chart (canvas xaxis yaxis &key xlabels ylabels box axesbox
                                  timeformat gmt axesatzero isometric)
  (make-instance 'strip-chart
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :xlabels xlabels
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt
                 :axesatzero axesatzero
                 :isometric isometric))

;; Table Chart

(defclass table-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart table-chart) &key columns widths)
  (format-wish "set ~a [::Plotchart::createTableChart ~a ~a ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (tcl-it columns)
               (tcl-it widths)))

(defun create-table-chart (canvas columns &key widths)
  "Creates an instance of a table-chart"
  (make-instance 'table-chart
                 :canvas canvas
                 :columns columns
                 :widths widths))

;; Ternary Diagram

(defclass ternary-diagram (plotchart)
  ())

(defmethod initialize-instance :after ((chart ternary-diagram) &key box axesbox fractions steps)
  (format-wish "set ~a [::Plotchart::createTernaryDiagram ~a ~a]"
               (name chart)
               (widget-path (canvas chart))
               (make-args (list (list "-box" box)
                                (list "-axesbox" axesbox)
                                (list "-fractions" (and fractions
                                                        (tcl-it-boolean fractions)))
                                (list "-steps" steps)))))
               
(defun create-ternary-diagram (canvas &key box axesbox fractions steps)
  "Creates an instance of a ternary-diagram"
  (make-instance 'ternary-diagram
                 :canvas canvas
                 :box box
                 :axesbox axesbox
                 :fractions fractions
                 :steps steps))

;; 3D Bar Chart

(defclass threed-bar-chart (plotchart)
  ((yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))
   (num-bars :accessor num-bars :initarg :num-bars :initform 1)))

(defmethod initialize-instance :after ((chart threed-bar-chart) &key yaxis (num-bars 1))
  (when (valid-axis-p yaxis "threed-bar-chart - yaxis")
    (format-wish "set ~a [::Plotchart::create3DBarchart ~a ~a ~d]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it yaxis)
                 num-bars)))

(defun create-3d-bar-chart (canvas yaxis &key (num-bars 1))
  "Creates an instance of a threed-bar-chart"
  (make-instance 'threed-bar-chart
                 :canvas canvas
                 :yaxis yaxis
                 :num-bars num-bars))

;; 3D Plot

(defclass threed-plot (plotchart)
  ())

(defmethod initialize-instance :after ((chart threed-plot) &key xaxis yaxis zaxis xlabels)
  (when (and (valid-axis-p xaxis "3d-plot - xaxis")
             (valid-axis-p yaxis "3d-plot - yaxis")
             (valid-axis-p zaxis "3d-plot - zaxis"))
    (format-wish "set ~a [::Plotchart::create3DPlot ~a ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis)
                 (tcl-it yaxis)
                 (tcl-it zaxis)
                 (if xlabels (format nil "-xlabels {~{ ~a~} }" xlabels) ""))))

(defun create-3d-plot (canvas xaxis yaxis zaxis &key xlabels)
  "Creates an instance of a 3d-plot"
  (make-instance 'threed-plot
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :zaxis zaxis
                 :xlabels xlabels))

;; 3D Ribbon Plot

(defclass threed-ribbon-plot (plotchart)
  ())

(defmethod initialize-instance :after ((chart threed-ribbon-plot) &key yaxis zaxis)
  (when (and (valid-axis-p yaxis "threed-ribbon-plot - yaxis")
             (valid-axis-p zaxis "threed-ribbon-plot - zaxis"))
    (format-wish "set ~a [::Plotchart::create3DRibbonPlot ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it yaxis) 
                 (tcl-it zaxis))))

(defun create-3d-ribbon-plot (canvas yaxis zaxis)
  "Creates an instance of a threed-ribbon-plot"
  (make-instance 'threed-ribbon-plot
                 :canvas canvas
                 :yaxis yaxis
                 :zaxis zaxis))

;; Time Chart

(defclass time-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart time-chart) &key time-begin time-end num-items bar-height ylabel-width)
  (format-wish "set ~a [::Plotchart::createTimechart ~a \"~a\" \"~a\" ~a~a~a]"
               (name chart)
               (widget-path (canvas chart))
               time-begin 
               time-end 
               (if num-items num-items "")
               (if bar-height (format nil " -barheight ~d" bar-height) "")
               (if ylabel-width (format nil " -ylabelwidth ~d" ylabel-width) "")))

(defun create-time-chart (canvas time-begin time-end &key num-items bar-height ylabel-width)
  (make-instance 'time-chart 
                 :canvas canvas
                 :time-begin time-begin
                 :time-end time-end
                 :num-items num-items
                 :bar-height bar-height
                 :ylabel-width ylabel-width))

;; TX Plot

(defclass tx-plot (plotchart)
  ())

(defmethod initialize-instance :after ((chart tx-plot) &key timeaxis xaxis
                                                       box
                                                       axesbox timeformat gmt
                                                       axesatzero isometric)
  (when (and (valid-time-axis-p timeaxis "tx-plot - timeaxis")
             (valid-axis-p xaxis "tx-plot - xaxis"))
    (format-wish "set ~a [::Plotchart::createTXPlot ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it timeaxis)
                 (tcl-it xaxis)
                 (make-xy-args nil nil box axesbox timeformat gmt axesatzero isometric))))

(defun create-tx-plot (canvas timeaxis xaxis &key box
                              axesbox timeformat gmt
                              axesatzero isometric)
  "Creates instance of a tx-plot"
  (make-instance 'tx-plot
                 :canvas canvas
                 :timeaxis timeaxis
                 :xaxis xaxis
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt
                 :axesatzero axesatzero
                 :isometric isometric))

;; Windrose

(defclass windrose (plotchart)
  ())

(defmethod initialize-instance :after ((chart windrose) &key radius-data (num-sectors 16))
  (when (valid-number-pair-p radius-data "windrose - radius-data")
    (format-wish "set ~a [::Plotchart::createWindRose ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it radius-data)
                 num-sectors)))

(defun create-windrose (canvas radius-data &key (num-sectors 16))
  "Creates an instance of a windrose"
  (make-instance 'windrose
                 :canvas canvas
                 :radius-data radius-data
                 :num-sectors num-sectors))

;; X-LogY Plot
;; - XY Plot with y axis having logarithmic values

(defclass x-logy-plot (xy-plot-group)
  ())

(defmethod initialize-instance :after ((chart x-logy-plot) &key 
                                                           xaxis yaxis
                                                           xlabels ylabels box
                                                           axesbox timeformat gmt
                                                           axesatzero isometric)
  (when (and (valid-axis-p xaxis "x-logy-plot - xaxis")
             (valid-axis-p yaxis "x-logy-plot - yaxis"))
    (format-wish "set ~a [::Plotchart::createXLogYPlot ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis)
                 (tcl-it yaxis)
                 (make-xy-args xlabels ylabels box axesbox timeformat gmt axesatzero isometric))))

(defun create-x-logy-plot (canvas xaxis yaxis &key xlabels ylabels box
                                  axesbox timeformat gmt
                                  axesatzero isometric)
  "Creates instance of an x-logy-plot"
  (make-instance 'x-logy-plot
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :xlabels xlabels
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt
                 :axesatzero axesatzero
                 :isometric isometric))

;; XY Plot

(defclass xy-plot (xy-plot-group) ())

(defmethod initialize-instance :after ((chart xy-plot) &key 
                                                       xaxis yaxis
                                                       xlabels ylabels box
                                                       axesbox timeformat gmt
                                                       axesatzero isometric)
  (when (and (valid-axis-p xaxis "xy-plot - xaxis")
             (valid-axis-p yaxis "xy-plot - yaxis"))
    (format-wish "set ~a [::Plotchart::createXYPlot ~a ~a ~a ~a]" 
                 (name chart) 
                 (widget-path (canvas chart))
                 (tcl-it xaxis) 
                 (tcl-it yaxis) 
                 (make-xy-args xlabels ylabels box axesbox timeformat gmt axesatzero isometric))))

(defun create-xy-plot (canvas xaxis yaxis &key xlabels ylabels box
                              axesbox timeformat gmt
                              axesatzero isometric)
  "Creates instance of an xy-plot"
  (make-instance 'xy-plot
                 :canvas canvas
                 :xaxis xaxis
                 :yaxis yaxis
                 :xlabels xlabels
                 :ylabels ylabels
                 :box box
                 :axesbox axesbox
                 :timeformat timeformat
                 :gmt gmt
                 :axesatzero axesatzero
                 :isometric isometric))

;; ---------------------------------------------------------------------------
;; Functions

(defgeneric add-milestone (chart time-point &optional colour))
(defmethod add-milestone ((chart time-chart) time-point &optional colour)
  "Adds another milestone to the current row of a chart"
  (format-wish "$~a addmilestone \"~a\" ~a" 
               (name chart) time-point (tcl-it colour)))

(defgeneric add-period (chart time-begin time-end &optional colour))
(defmethod add-period ((chart time-chart) time-begin time-end &optional colour)
  "Adds a new time period to the current row of a chart"
  (format-wish "$~a addperiod \"~a\" \"~a\" ~a" 
               (name chart) time-begin time-end
               (tcl-it colour)))

(defgeneric add-row (chart items))
(defmethod add-row ((chart table-chart) items)
  "Adds a row of information to chart"
  (format-wish "$~a row ~a" (name chart) (tcl-it items)))

(defgeneric background (chart part colour-or-image &optional direction brightness))
(defmethod background ((chart plotchart) part colour-or-image &optional direction (brightness :bright))
  "Sets the background of a part of the plot"
  (format-wish "$~a background ~a ~a ~a ~a"
               (name chart) 
               (tcl-it part)
               (if (eql :image part)
                 (widget-path colour-or-image)
                 (tcl-it colour-or-image))
               (tcl-it direction)
               (tcl-it brightness)))

(defgeneric balloon (chart x y text direction))
(defmethod balloon ((chart plotchart) x y text direction)
  "Adds balloon text to the plot, with pointer to given coordinates"
  (format-wish "$~a balloon ~f ~f \"~a\" ~a" 
               (name chart) x y text 
               (if direction
                 (tcl-it-checked direction 
                                 '("north" "north-east" "east" "south-east" "south"
                                   "south-west" "west" "north-west")
                                 "direction")
                 "")))

(defgeneric balloon-config (chart &key font justify textcolour textcolor 
                                  background outline margin rimwidth arrowsize))
(defmethod balloon-config ((chart plotchart) &key font justify textcolour 
                                             textcolor background outline margin 
                                             rimwidth arrowsize)
  "Configures balloon text for given plot - settings apply to next call to chart-balloon"
  (format-wish "$~a balloonconfig ~a" 
               (name chart)
               (make-args (list (list "-font" font)
                                (list "-justify" 
                                      (and justify
                                           (tcl-it-checked justify 
                                                           '("left" "center" "right") 
                                                           "balloon-config - justify")))
                                (list "-textcolour" (or textcolour textcolor))
                                (list "-background" background)
                                (list "-outline" outline)
                                (list "-margin" margin)
                                (list "-rimwidth" rimwidth)
                                (list "-arrowsize" arrowsize)))))

(defgeneric cell-configure (chart &key background colour color font anchor justify))
(defmethod cell-configure ((chart table-chart) &key background colour color font anchor justify)
  (format-wish "$~a cellconfigure ~a"
               (name chart)
               (make-args (list (list "-background" background)
                                (list "-color" (or colour color))
                                (list "-font" font)
                                (list "-anchor" anchor)
                                (list "-justify" justify)))))

(defgeneric color (chart item border))
(defmethod color (chart item border)
  "Passes to 'colour' variant"
  (colour chart item border))

(defgeneric colors (chart &rest colours))
(defmethod colors (chart &rest colours)
  "Passes to 'colour' variant"
  (apply #'colours chart colours))

(defun color-map (colours)
  (colour-map colours))

(defgeneric colour (chart fill border))
(defmethod colour ((chart threed-plot) fill border)
  "Sets the fill and border colour"
  (format-wish "$~a colour ~a ~a" 
               (name chart) (tcl-it fill) (tcl-it border)))

(defmethod colour ((chart gantt-chart) keyword newcolour)
  (format-wish "$~a colour ~a ~a" 
               (name chart) 
               (tcl-it-checked keyword
                          '("description" "completed" "left" "odd" "even" "summary" "summarybar")
                          "keyword")
               (tcl-it newcolour)))

(defun colours (chart &rest colours)
  "Sets the colours for the pie slices or radial chart spokes"
  (typecase chart
    ((or pie-chart spiral-pie)
     (format-wish "$~a colours ~{~a ~}" 
                  (name chart)
                  (mapcar #'(lambda (c) (string-downcase (string c))) colours)))
    (otherwise
      (error "Unknown type ~a passed to colours" (type-of chart)))))

(defun colour-map (colours)
  "Sets the colours to use with the contour map methods"
  (format-wish "::Plotchart::colorMap ~a" (tcl-it colours)))

(defun config (chart &key show-values value-font value-colour value-format
                     use-background use-ticklines
                     label-font label-colour)
  "Sets configuration parameters for bar charts"
  (typecase chart
    ((or bar-chart horizontal-bar-chart)
     (when (or use-background use-ticklines label-font label-colour)
       (error "Unknown option given to config for chart type ~a" (type-of chart)))
     (format-wish "$~a config ~a"
                  (name chart)
                  (make-args (list (list "-showvalues" (and show-values
                                                            (tcl-it-boolean show-values)))
                                   (list "-valuefont" value-font)
                                   (list "-valuecolour" value-colour)
                                   (list "-valueformat" value-format)))))
    (threed-bar-chart
      (when value-format
        (error "Unknown option given to config for chart type ~a" (type-of chart)))
      (format-wish "$~a config ~a"
                  (name chart)
                  (make-args (list (list "-showvalues" (and show-values
                                                            (tcl-it-boolean show-values)))
                                   (list "-valuefont" value-font)
                                   (list "-valuecolour" value-colour)
                                   (list "-usebackground" (and use-background
                                                               (tcl-it-boolean use-background)))
                                   (list "-useticklines" (and use-ticklines
                                                              (tcl-it-boolean use-ticklines)))
                                   (list "-labelfont" label-font)
                                   (list "-labelcolour" label-colour)))))

    (otherwise
      (error "Unknown chart type ~a passed to config" (type-of chart)))))

(defgeneric connect (chart from to))
(defmethod connect ((chart gantt-chart) from to)
  (format-wish "$~a connect $~a $~a" 
               (name chart) from to))

(defgeneric corner-text (chart xtext ytext ztext))
(defmethod corner-text ((chart ternary-diagram) xtext ytext ztext)
  "Displays given text at each corner"
  (format-wish "$~a text \"~a\" \"~a\" \"~a\""
               (name chart) xtext ytext ztext))

(defun data-config (chart series &key colour color type symbol radius width filled fillcolour style smooth
                          boxwidth whiskers whiskerwidth mediancolour medianwidth)
  "Sets the configuration for drawing of data in a given series, for specified chart types"
  (typecase chart
    ((or box-plot histogram polar-plot right-axis ternary-diagram tx-plot xy-plot-group)
     (format-wish "$~a dataconfig ~a ~a"
                  (name chart)
                  series
                  (make-config-args colour color type symbol radius width filled fillcolour style smooth
                                    boxwidth whiskers whiskerwidth mediancolour medianwidth)))
    (otherwise
      (error "Unknown chart type ~a passed to data-config" (type-of chart)))))

(defun dot-config (chart series &key colour color scale radius scalebyvalue outline classes effect-3d)
  "Configuration options for drawing dots on plots"
  (typecase chart
    ((or xy-plot-group polar-plot)
     (format-wish "$~a dotconfig ~a ~a"
                  (name chart) series
                  (make-args (list (list "-colour" (or colour color))
                                   (list "-scale" scale)
                                   (list "-radius" radius)
                                   (list "-scalebyvalue" (tcl-it-boolean scalebyvalue))
                                   (list "-outline" outline)
                                   (list "-classes" classes)
                                   (list "-3deffect" (and effect-3d (tcl-it-boolean effect-3d)))))))
    (otherwise
      (error "Unknown chart type ~a passed to dot-config" (type-of chart)))))

(defgeneric draw-box-and-whiskers (chart series xcrd ycrd))
(defmethod draw-box-and-whiskers ((chart xy-plot-group) series xcrd ycrd)
  "Draw a box and whiskers in the plot."
  (if (and (listp xcrd) (listp ycrd))
    (error "box-and-whiskers: only one of xcrd/ycrd can be a list")
    (format-wish "$~a box-and-whiskers ~a ~a ~a"
                 (name chart) series (tcl-it xcrd) (tcl-it ycrd))))

(defgeneric draw-circle (chart xc yc radius &optional colour))
(defmethod draw-circle ((chart isometric-plot) xc yc radius &optional colour)
  "Draw the outlines of specified circle on isometric-plot"
  (format-wish "$~a plot circle ~f ~f ~f ~a" (name chart) xc yc radius 
               (tcl-it colour)))

(defgeneric draw-contour-box (chart xcrds ycrds values &optional classes))
(defmethod draw-contour-box ((chart xy-plot-group) xcrds ycrds values &optional classes)
  "Draws cells as filled rectangles for the given values on the grid"
  (format-wish "$~a contourbox ~a ~a ~a ~a"
               (name chart) (tcl-it xcrds) (tcl-it ycrds) 
               (tcl-it values) (tcl-it classes)))

(defgeneric draw-contour-fill (chart xcrds ycrds values &optional classes))
(defmethod draw-contour-fill ((chart xy-plot-group) xcrds ycrds values &optional classes)
  "Draws filled contours for the given values on the grid"
  (format-wish "$~a contourfill ~a ~a ~a ~a"
               (name chart) (tcl-it xcrds) (tcl-it ycrds) 
               (tcl-it values) (tcl-it classes)))

(defgeneric draw-contour-lines (chart xcrds ycrds values &optional classes))
(defmethod draw-contour-lines ((chart xy-plot-group) xcrds ycrds values &optional classes)
  "Draws contour lines for the given values on the grid"
  (format-wish "$~a contourlines ~a ~a ~a ~a"
               (name chart) (tcl-it xcrds) (tcl-it ycrds) 
               (tcl-it values) (tcl-it classes)))

(defgeneric draw-contour-lines-function-values (chart xvec yvec valuesmat &optional classes))
(defmethod draw-contour-lines-function-values ((chart xy-plot-group) xvec yvec valuesmat &optional classes)
  "Draws contour lines for the given values on the grid"
  (format-wish "$~a contourlinesfunctionvalues ~a ~a ~a ~a"
               (name chart) (tcl-it xvec) (tcl-it yvec) 
               (tcl-it valuesmat) (tcl-it classes)))

(defgeneric draw-dot (chart series xcrd ycrd value))
(defmethod draw-dot ((chart xy-plot-group) series xcrd ycrd value)
  "Draws a dot in the given plot"
  (format-wish "$~a dot ~a ~f ~f ~f"
               (name chart) series xcrd ycrd value))

(defmethod draw-dot ((chart polar-plot) series xcrd ycrd value)
  "Draws a dot in the given plot"
  (format-wish "$~a dot ~a ~f ~f ~f"
               (name chart) series xcrd ycrd value))

(defgeneric draw-filled-circle (chart xc yc radius &optional colour))
(defmethod draw-filled-circle ((chart isometric-plot) xc yc radius &optional colour)
  "Draw and fill specified circle on an isometric-plot"
  (format-wish "$~a plot filled-circle ~f ~f ~f ~a" (name chart) xc yc radius 
               (if colour (string-downcase (string colour)) "")))

(defgeneric draw-filled-polygon (chart series coords))
(defmethod draw-filled-polygon ((chart ternary-diagram) series coords)
  "Fill a polygon defined by series of coordinates (triplets)"
  (format-wish "$~a fill ~a ~a"
               (name chart) 
               series 
               (tcl-it (reduce #'append coords))))

(defgeneric draw-filled-rectangle (chart x1 y1 x2 y2 &optional colour))
(defmethod draw-filled-rectangle ((chart isometric-plot) x1 y1 x2 y2 &optional colour)
  "Draw and fill specified rectangle on an isometric-plot"
  (format-wish "$~a plot filled-rectangle ~f ~f ~f ~f ~a" (name chart) x1 y1 x2 y2 
               (tcl-it colour)))

(defgeneric draw-grid (chart xcrds ycrds))
(defmethod draw-grid ((chart xy-plot-group) xcrds ycrds)
  "Draws the grid cells as lines connecting the given coordinates"
  (format-wish "$~a grid ~a ~a" (name chart) (tcl-it xcrds) (tcl-it ycrds)))


(defgeneric draw-interval (chart series xcoord ymin ymax &optional ycenter))
(defmethod draw-interval ((chart xy-plot-group) series xcoord ymin ymax &optional ycenter)
  "Adds a vertical error interval to xy-plot"
  (format-wish "$~a interval ~a ~f ~f ~f ~a"
               (name chart) series xcoord ymin ymax
               (if ycenter ycenter "")))

(defmethod draw-interval ((chart tx-plot) series xcoord ymin ymax &optional ycenter)
  "Adds a vertical error interval to tx-plot"
  (format-wish "$~a interval ~a ~f ~f ~f ~a"
               (name chart) series xcoord ymin ymax
               (if ycenter ycenter "")))

(defgeneric draw-label-dot (chart x y text &optional orientation))
(defmethod draw-label-dot ((chart xy-plot-group) x y text &optional orientation)
  "Draws a label and dot in the given plot: configure using data-config using 'labeldot' as series name"
  (format-wish "$~a labeldot ~f ~f \"~a\" ~a"
               (name chart) x y text
               (and orientation 
                    (tcl-it-checked orientation '("n" "s" "e" "w") "orientation"))))

(defmethod draw-label-dot ((chart polar-plot) x y text &optional orientation)
  "Draws a label and dot in the given plot: configure using data-config using 'labeldot' as series name"
  (format-wish "$~a labeldot ~f ~f \"~a\" ~a"
               (name chart) x y text
               (and orientation
                    (tcl-it-checked orientation '("n" "s" "e" "w") "orientation"))))

(defgeneric draw-line (chart series coords))
(defmethod draw-line ((chart ternary-diagram) series coords)
  "Draw a continuous line given a series of coordinates (triplets)"
  (format-wish "$~a line ~a ~a"
               (name chart) 
               series 
               (tcl-it (reduce #'append coords))))

(defgeneric draw-minmax (chart series xcoord ymin ymax))
(defmethod draw-minmax ((chart xy-plot-group) series xcoord ymin ymax)
  "Draws a filled strip representing a minimum and maximum"
  (format-wish "$~a minmax ~a ~f ~f ~f"
               (name chart) series xcoord ymin ymax))

(defmethod draw-minmax ((chart polar-plot) series xcoord ymin ymax)
  "Draws a filled strip representing a minimum and maximum"
  (format-wish "$~a minmax ~a ~f ~f ~f"
               (name chart) series xcoord ymin ymax))

(defgeneric draw-rectangle (chart x1 y1 x2 y2 &optional colour))
(defmethod draw-rectangle ((chart isometric-plot) x1 y1 x2 y2 &optional colour)
  "Draw the outlines of specified rectangle on an isometric-plot"
  (format-wish "$~a plot rectangle ~f ~f ~f ~f ~a" (name chart) x1 y1 x2 y2 
               (tcl-it colour)))

(defgeneric draw-region (chart series xlist ylist))
(defmethod draw-region ((chart xy-plot-group) series xlist ylist)
  "Draws a filled polygon"
  (format-wish "$~a region ~a ~a ~a"
               (name chart)
               series
               (tcl-it xlist)
               (tcl-it ylist)))

(defmethod draw-region ((chart polar-plot) series xlist ylist)
  "Draws a filled polygon"
  (format-wish "$~a region ~a ~a ~a"
               (name chart)
               series
               (tcl-it xlist)
               (tcl-it ylist)))

(defgeneric draw-ticklines (chart &optional colour))
(defmethod draw-ticklines ((chart ternary-diagram) &optional colour)
  "Displays ticklines in optional colour"
  (format-wish "$~a ticklines ~a" (name chart) (tcl-it colour)))

(defgeneric draw-trendline (chart series xcoord ycoord))
(defmethod draw-trendline ((chart xy-plot-group) series xcoord ycoord)
  "Trend line for xy-style plots"
  (format-wish "$~a trend ~a ~f ~f"
               (name chart) series xcoord ycoord))

(defgeneric draw-vector (chart series xcrd ycrd ucmp vcmp))
(defmethod draw-vector ((chart xy-plot-group) series xcrd ycrd ucmp vcmp)
  "Draws a vector in the given plot"
  (format-wish "$~a vector ~a ~f ~f ~f ~f"
               (name chart) series xcrd ycrd ucmp vcmp))

(defun draw-vertical-line (chart text time-point &key color colour dash fill width)
  "Adds a vertical line to a chart"
  (typecase chart
    ((or gantt-chart time-chart)
     (when (or dash fill width)
       (warn "Chart type ~a does not support 'dash' 'fill' 'width' options"
             (type-of chart)))
     (format-wish "$~a vertline \"~a\" \"~a\" ~a" 
                  (name chart) text time-point (tcl-it (or color colour))))
    (status-timeline
      (format-wish "$~a vertline \"~a\" ~f ~a"
                   (name chart) text time-point
                   (make-args (list (list "-colour" (or color colour))
                                    (list "-dash" dash)
                                    (list "-fill" fill)
                                    (list "-width" width)))))
    (otherwise
      (error "Unknown chart type ~a passed to vertical-line" (type-of chart)))))

(defgeneric draw-x-band (chart ymin ymax))
(defmethod draw-x-band ((chart plotchart) ymin ymax)
  "Draws a horizontal light-grey band"
  (format-wish "$~a xband ~f ~f" (name chart) ymin ymax))

(defgeneric draw-y-band (chart xmin xmax))
(defmethod draw-y-band ((chart plotchart) xmin xmax)
  "Draws a vertical light-grey band"
  (format-wish "$~a yband ~f ~f" (name chart) xmin xmax))

(defgeneric explode (chart segment))
(defmethod explode ((chart pie-chart) segment)
  "Displays segment number out of the circle"
  (format-wish "$~a explode ~d" (name chart) segment))

(defgeneric font (chart keyword newfont))
(defmethod font ((chart gantt-chart) keyword newfont)
  (format-wish "$~a font ~a \"~a\"" 
               (name chart) 
               (tcl-it keyword)
               newfont))

(defgeneric gridsize (chart nxcells nycells))
(defmethod gridsize ((chart threed-plot) nxcells nycells)
  "Sets the grid resolution in the 3d plot"
  (format-wish "$~a gridsize ~d ~d" (name chart) nxcells nycells))

(defun horizontal-scrollbar (chart hscroll)
  "Connect a horizontal scrollbar"
  (typecase chart
    ((or gantt-chart time-chart)
     (format-wish "$~a hscroll ~a" (name chart) (name hscroll)))
    (otherwise
      (error "Unknown chart type ~a passed to horizontal-scrollbar" (type-of chart)))))

(defgeneric interpolate-data (chart data contours))
(defmethod interpolate-data ((chart threed-plot) data contours)
  "Plots given list-of-lists data with interpolated contours"
  (format-wish "$~a interpolatedata ~a ~a"
               (name chart)
               (tcl-it data)
               (tcl-it contours)))

(defgeneric legend (chart series text &optional spacing))
(defmethod legend ((chart plotchart) series text &optional spacing)
  "Adds an entry to the chart legend"
  (format-wish "$~a legend ~a \"~a\" ~a" 
               (name chart) series text (if spacing spacing "")))

(defgeneric legend-config (chart &key background border canvas font legend-type position spacing))
(defmethod legend-config ((chart plotchart) &key background border canvas font legend-type position spacing)
  (format-wish "$~a legendconfig ~a" 
               (name chart) 
               (make-args (list (list "-background" background)
                                (list "-border" border)
                                (list "-canvas" (widget-path canvas))
                                (list "-font" font)
                                (list "-legendtype" legend-type)
                                (list "-position" position)
                                (list "-spacing" spacing)))))

(defgeneric legend-remove (chart series))
(defmethod legend-remove ((chart plotchart) series)
  "Removes series from legend and redraws it"
  (format-wish "$~a removefromlegend \"~a\"" (name chart) series))

(defgeneric legend-isometric-lines (chart values classes))
(defmethod legend-isometric-lines ((chart xy-plot-group) values classes)
  "Adds contour classes to the legend as coloured lines"
  (format-wish "$~s legendisolines ~a ~a" (name chart) (tcl-it values) (tcl-it classes)))

(defgeneric legend-shades (chart values classes))
(defmethod legend-shades ((chart xy-plot-group) values classes)
  "Adds contour classes to the legend as coloured rectangles"
  (format-wish "$~s legendshades ~a ~a" (name chart) (tcl-it values) (tcl-it classes)))

(defun milestone (chart text time &optional colour)
  "Adds a milestone to a chart"
  (typecase chart
    ((or gantt-chart time-chart)
     (format-wish "$~a milestone \"~a\" \"~a\" ~a" 
                  (name chart) text time (tcl-it colour)))
    (otherwise
      (error "Unknown chart type ~a passed to milestone" (type-of chart)))))

(defgeneric period (chart text time-begin time-end &optional colour))
(defmethod period ((chart time-chart) text time-begin time-end &optional colour)
  "Adds a time period to a chart"
  (format-wish "$~a period \"~a\" \"~a\" \"~a\" ~a" 
               (name chart) text time-begin time-end
               (tcl-it colour)))

(defmethod period ((chart gantt-chart) text time-begin time-end &optional colour)
  "Adds a time period to a chart"
  (format-wish "$~a period \"~a\" \"~a\" \"~a\" ~a" 
               (name chart) text time-begin time-end
               (tcl-it colour)))

(defgeneric plaintext (chart x y text &optional direction))
(defmethod plaintext ((chart plotchart) x y text &optional (direction "north"))
  "Adds plaintext to the plot, at given coordinates"
  (format-wish "$~a plaintext ~f ~f \"~a\" \"~a\"" 
               (name chart) x y text 
               (tcl-it-checked direction  
                               '("north" "north-east" "east" "south-east" "south"
                                 "south-west" "west" "north-west")
                               "direction")))

(defgeneric plaintext-config (chart &key font justify text-colour text-color)) 
(defmethod plaintext-config ((chart plotchart) &key font justify text-colour text-color)
  "Configures plaintext for given plot - settings apply to next call to chart-balloon"
  (format-wish "$~a plaintextconfig ~a" 
               (name chart) 
               (make-args (list (list "-font" font)
                                (list "-justify" (and justify
                                                      (tcl-it-checked justify '("left" "center" "right") "plaintext-config - justify")))
                                (list "-textcolour" (or text-colour text-color))))))

(defun plot (chart &rest args)
  "Generic call"
  (typecase chart
    ((or bar-chart horizontal-bar-chart)
     (if (member (length args) '(3 4 5))
       (format-wish "$~a plot ~a ~a ~a ~a ~a"
                    (name chart)
                    (first args)
                    (tcl-it (second args))
                    (tcl-it (third args))
                    (if (>= (length args) 4) (tcl-it (fourth args)) "")
                    (if (= (length args) 5) (tcl-it (fifth args)) ""))
       (error "bar-chart:plot series xdata colour &optional direction brightness")))

    (box-plot
      (if (and (= 3 (length args))
               (listp (third args)))
        (format-wish "$~a plot ~a ~a ~a" 
                     (name chart) (first args) (tcl-it (second args)) (tcl-it (third args)))
        (error "box-plot:plot series value labels")))

    (histogram
      (if (= 3 (length args))
        (format-wish "$~a plot ~a ~f ~f" (name chart) (first args) (second args) (third args))
        (error "histogram:plot series x-coord y-coord")))

    ((or pie-chart spiral-pie)
     (if (and (= 1 (length args))
              (listp (first args)))
       (format-wish "$~a plot ~a" (name chart) (tcl-it (reduce #'append (first args))))
       (error "plot requires 1 list argument for pie-chart")))

    ((or polar-plot right-axis xy-plot-group)
     (if (= 3 (length args))
       (format-wish "$~a plot ~a ~f ~f" (name chart) (first args) (second args) (third args))
       (error "plot requires 3 arguments for ~a" (type-of chart))))

    (radial-chart
      (if (member (length args) '(2 3))
        (format-wish "$~a plot ~a ~a ~a" (name chart)
                     (tcl-it (first args)) 
                     (tcl-it (second args))
                     (if (= 3 (length args)) (third args) ""))
        (error "radial-chart:plot data colour &optional thickness")))

    (status-timeline
      (if (member (length args) '(3 4))
        (format-wish "$~a plot ~a ~f ~f ~a"
                     (name chart) (first args) (second args) (third args) 
                     (if (= 4 (length args)) (tcl-it (fourth args)) ""))
        (error "status-timeline:plot series start stop &optional colour")))

    (ternary-diagram
      (if (member (length args) '(5 6))
        (format-wish "$~a plot ~a ~f ~f ~f \"~a\" ~a"
                     (name chart) (first args) (second args) (third args) (fourth args) (fifth args)
                     (if (= 6 (length args)) 
                       (tcl-it-checked (sixth args) 
                                       '("n" "s" "e" "w" "nw" "ne" "sw" "se")
                                       "plot -ternary-diagram position")
                       ""))
        (error "ternary-diagram:plot series xcrd ycrd zcrd text &optional direction")))

    (threed-bar-chart
      (if (= 3 (length args))
        (format-wish "$~a plot ~a ~a ~a" 
                     (name chart) 
                     (first args)
                     (second args)
                     (tcl-it (third args)))
        (error "threed-bar-chart:plot label yvalue colour")))

    (threed-ribbon-plot
      (if (= 1 (length args))
        (format-wish "$~a plot ~a"
                     (name chart)
                     (tcl-it (first args)))
        (error "threed-ribbon-plot:plot yzpairs")))

    (tx-plot
      (if (and (= 3 (length args)) (stringp (second args)) (numberp (third args)))
        (format-wish "$~a plot ~a ~a ~f" (name chart) (first args) (second args) (third args))
        (error "plot requires 3 arguments (series time-coord x-coord) for tx-plot, not ~a" args)))

    (windrose
      (if (= 2 (length args))
        (format-wish "$~a plot ~a ~a" (name chart) (tcl-it (first args)) (tcl-it (second args)))
        (error "windrose:plot data colour")))

    (otherwise
      (error "Unknown chart type passed to plot"))))

(defun plot-config (charttype component property value)
  "Sets a new value for given charttype-component-property"
  (format-wish "::Plotchart::plotconfig ~a ~a ~a ~a"
               (tcl-it charttype) (tcl-it component) (tcl-it property) (tcl-it value)))

(defgeneric plot-cumulative (chart series x-coord y-coord))
(defmethod plot-cumulative ((chart histogram) series x-coord y-coord)
  "Adds a data point to the chart, accumulating previous points"
  (format-wish "$~a plotcumulative ~a ~f ~f" (name chart) series x-coord y-coord))

(defgeneric plot-data (chart data))
(defmethod plot-data ((chart threed-plot) data)
  "Plots given list-of-lists data"
  (format-wish "$~a plotdata ~a"
               (name chart)
               (tcl-it data)))

(defgeneric plot-erase (chart))
(defmethod plot-erase ((chart plotchart))
  "Erases given chart and all associated resources"
  (format-wish "::Plotchart::eraseplot $~a" (name chart)))

(defgeneric plot-list (chart series xlist ylist &optional every))
(defmethod plot-list ((chart xy-plot-group) series xlist ylist &optional every)
  "Draws a series of data as a whole"
  (format-wish "$~a plotlist ~a ~a ~a ~a"
               (name chart)
               (tcl-it series)
               (tcl-it xlist)
               (tcl-it ylist)
               (if every every "")))

(defgeneric plot-pack (canvas direction &rest charts))
(defmethod plot-pack ((canvas canvas) direction &rest charts)
  "Copies contents of charts onto given canvas"
  (format-wish "::Plotchart::plotpack ~a ~a ~{ $~a~}"
               (widget-path canvas)
               (tcl-it-checked direction '("top" "left" "bottom" "right") "direction")
               (mapcar #'name charts)))

(defgeneric rchart (chart series xcoord ycoord))
(defmethod rchart ((chart xy-plot-group) series xcoord ycoord)
  "Like plot, but adds +/- s.d. line"
  (format-wish "$~a rchart ~a ~f ~f"
               (name chart) series xcoord ycoord))

(defgeneric ribbon (chart yzpairs))
(defmethod ribbon ((chart threed-plot) yzpairs)
  "Plots a ribbon based on given yz-pairs"
  (format-wish "$~a ribbon ~a"
               (name chart)
               (tcl-it yzpairs)))

(defgeneric save-plot (chart filename &key plotregion))
(defmethod save-plot ((chart plotchart) filename &key (plotregion :window))
  "Saves the chart to a postscript file"
  (format-wish "$~a saveplot \"~a\" -plotregion ~a" 
               (name chart) 
               filename 
               (tcl-it plotregion)))

(defgeneric separator (chart))
(defmethod separator ((chart table-chart))
  "Adds a separating line to chart"
  (format-wish "$~a separator" (name chart)))

(defgeneric subtitle (chart title))
(defmethod subtitle ((chart plotchart) title)
  "Sets the chart subtitle"
  (format-wish "$~a subtitle \"~a\"" (name chart) title))

(defgeneric summary (chart text &rest args))
(defmethod summary ((chart gantt-chart) text &rest args)
  (format-wish "$~a summary \"~a\" ~{ $~a~}" 
               (name chart) text args))

(defgeneric task (chart text time-begin time-end completed))
(defmethod task ((chart gantt-chart) text time-begin time-end completed)
  (let ((task-name (new-name)))
    (format-wish "set ~a [$~a task \"~a\" \"~a\" \"~a\" ~f]" 
                 task-name
                 (name chart) text time-begin time-end completed)
    task-name))

(defgeneric title-text (chart title &optional placement))
(defmethod title-text ((chart plotchart) title &optional (placement "center"))
  "Sets the chart title: placement can be :center, :left or :right"
  (let ((place (tcl-it-checked placement '("center" "left" "right") "title-text: placement")))
    (format-wish "$~a title \"~a\" ~a" (name chart) title place)))

(defgeneric v-subtext (chart text))
(defmethod v-subtext ((chart plotchart) text)
  "Sets the subtext of the (vertical) y-axis, and displays vertically along axis"
  (format-wish "$~a vsubtext \"~a\"" (name chart) text))

(defgeneric v-text (chart text))
(defmethod v-text ((chart plotchart) text)
  "Sets the title of the (vertical) y-axis, and displays vertically along axis"
  (format-wish "$~a vtext \"~a\"" (name chart) text))

(defgeneric vector-config (chart series &key colour color scale centred type))
(defmethod vector-config ((chart xy-plot-group) series &key colour color scale centred centered type)
  "Configuration options for drawing vectors on xy-plots"
  (when colour
    (format-wish "$~a vectorconfig ~a -colour ~a" (name chart) series (tcl-it colour)))
  (when color
    (format-wish "$~a vectorconfig ~a -colour ~a" (name chart) series (tcl-it color)))
  (when scale
    (format-wish "$~a vectorconfig ~a -scale ~a" (name chart) series (tcl-it scale)))
  (when centred
    (format-wish "$~a vectorconfig ~a -centred ~a" (name chart) series (tcl-it centred)))
  (when centered
    (format-wish "$~a vectorconfig ~a -centred ~a" (name chart) series (tcl-it centered)))
  (when type
    (format-wish "$~a vectorconfig ~a -type ~a" 
                 (name chart) series 
                 (tcl-it-checked type '("cartesian" "polar" "nautical") "type"))))

(defun vertical-scrollbar (chart vscroll)
  "Connect a vertical scrollbar"
  (typecase chart
    ((or gantt-chart time-chart)
     (format-wish "$~a vscroll ~a" (name chart) (name vscroll)))
    (otherwise
      (error "Unknown chart type ~a passed to vertical-scrollbar" (type-of chart)))))

(defgeneric x-config (chart &key format tick-length tick-lines minor-ticks label-offset scale))
(defmethod x-config ((chart plotchart) &key format tick-length tick-lines minor-ticks label-offset scale)
  (format-wish "$~a xconfig ~a"
               (name chart)
               (make-args (list (list "-format" format)
                                (list "-ticklength" tick-length)
                                (list "-ticklines" (and tick-lines
                                                        (tcl-it-boolean tick-lines)))
                                (list "-minorticks" minor-ticks)
                                (list "-labeloffset" label-offset)
                                (list "-scale" scale)))))

(defgeneric x-subtext (chart text))
(defmethod x-subtext ((chart plotchart) text)
  "Sets the subtext of the (horizontal) x-axis"
  (format-wish "$~a xsubtext \"~a\"" (name chart) text))

(defgeneric x-text (chart text))
(defmethod x-text ((chart plotchart) text)
  "Sets the title of the (horizontal) x-axis"
  (format-wish "$~a xtext \"~a\"" (name chart) text))

(defgeneric x-ticklines (chart &optional colour dash))
(defmethod x-ticklines ((chart plotchart) &optional (colour "black") (dash :lines))
  "Draw vertical ticklines at each tick location"
  (format-wish "$~a xticklines ~a ~a"
               (name chart)
               (tcl-it colour)
               (tcl-it-checked dash '("lines" "dots1" "dots2" "dots3" "dots4" "dots5") "x-ticklines - dash")))

(defgeneric y-config (chart &key format tick-length tick-lines minor-ticks label-offset scale))
(defmethod y-config ((chart plotchart) &key format tick-length tick-lines minor-ticks label-offset scale)
  (format-wish "$~a yconfig ~a"
               (name chart)
               (make-args (list (list "-format" format)
                                (list "-ticklength" tick-length)
                                (list "-ticklines" (and tick-lines
                                                        (tcl-it-boolean tick-lines)))
                                (list "-minorticks" minor-ticks)
                                (list "-labeloffset" label-offset)
                                (list "-scale" scale)))))

(defgeneric y-subtext (chart text))
(defmethod y-subtext ((chart plotchart) text)
  "Sets the subtext of the (vertical) y-axis"
  (format-wish "$~a ysubtext \"~a\"" (name chart) text))

(defgeneric y-text (chart text))
(defmethod y-text ((chart plotchart) text)
  "Sets the title of the (vertical) y-axis"
  (format-wish "$~a ytext \"~a\"" (name chart) text))

(defgeneric y-ticklines (chart &optional colour dash))
(defmethod y-ticklines ((chart plotchart) &optional (colour "black") (dash :lines))
  "Draw horizontal ticklines at each tick location"
  (format-wish "$~a yticklines ~a ~a"
               (name chart)
               (tcl-it colour)
               (tcl-it-checked dash '("lines" "dots1" "dots2" "dots3" "dots4" "dots5") "y-ticklines - dash")))

;; ---------------------------------------------------------------------------
;; Utility functions - internal use only

(defun tcl-it (value)
  "Returns a string representation of given value, in TCL form"
  (cond 
    ((null value)
     "")
    (t 
      (typecase value
        (string ; multi-word strings should be in quotes, but otherwise leave as is
          (if (position #\space value)
            (uiop:strcat "\"" value "\"")
            value))
        ((or keyword symbol) ; keywords/symbols should be lower-case strings
         (string-downcase (string value)))
        (float
          (format nil "~f" value))
        (integer 
          (format nil "~d" value))
        (list
          (format nil "{~{ ~a~} }" (mapcar #'tcl-it value)))
        (otherwise
          (error "Unknown value type for tcl-it"))
        ))))

(defun tcl-it-boolean (value)
  "Returns a string representation of value, treated as boolean"
  (if value "1" "0"))

(defun tcl-it-checked (value valid-values name)
  "Converts value and then checks it is a valid value before returning it - errors if not"
  (let ((result (string-downcase ; this is only used for tcl options, so use lowercase
                  (tcl-it value))))
    (if (member result valid-values :test #'string=)
      result
      (error "Value ~a is not a valid ~a" result name))))

(defun make-args (pairs)
  "Returns a tcl option string for given pairs of config option-value pairs"
  (reduce #'uiop:strcat
          (mapcar #'(lambda (pair)
                      (if (second pair)
                        (format nil "~a ~a " (first pair) (tcl-it (second pair)))
                        ""))
                  pairs)))

(defun make-config-args (colour color type symbol radius width filled
                                fillcolour style &optional smooth
                                boxwidth whiskers whiskerwidth mediancolour medianwidth)
  "Returns a tcl option string for given config arguments"
  (make-args (list (list "-colour" (or colour color))
                   (list "-type" type)
                   (list "-symbol" symbol)
                   (list "-radius" radius)
                   (list "-width" width)
                   (list "-filled" filled)
                   (list "-fillcolour" fillcolour)
                   (list "-style" style)
                   (list "-smooth" (and smooth
                                        (tcl-it-boolean smooth)))
                   (list "-boxwidth" boxwidth)
                   (list "-whiskers" whiskers)
                   (list "-whiskerwidth" whiskerwidth)
                   (list "-mediancolour" mediancolour)
                   (list "-medianwidth" medianwidth))))

(defun make-xy-args (xlabels ylabels box axesbox timeformat gmt &optional axesatzero isometric)
  "Returns a tcl option string for given xy arguments"
  (make-args (list (list "-xlabels" xlabels)
                   (list "-ylabels" ylabels)
                   (list "-box" box)
                   (list "-axesbox" axesbox)
                   (list "-timeformat" timeformat)
                   (list "-gmt" (and gmt (tcl-it-boolean gmt)))
                   (list "-axesatzero" (and axesatzero (tcl-it-boolean axesatzero)))
                   (list "-isometric" (and isometric (tcl-it-boolean isometric))))))

(defun valid-axis-p (axis name)
  "Checks the given axis is a valid len-item list of numbers - throws error if invalid"
  (or (and (listp axis)
           (= 3 (length axis))
           (numberp (first axis))
           (numberp (second axis))
           (or (numberp (third axis))
               (string= "" (third axis))))
      (error "Given axis ~a is not a valid axis for ~a" axis name)))

(defun valid-number-pair-p (data name)
  "Checks data is a valid 2-item list of numbers - throws error if invalid"
  (or (and (listp data)
           (= 2 (length data))
           (numberp (first data))
           (numberp (second data)))
      (error "Given data ~a is not valid for ~a" data name)))

(defun valid-time-axis-p (axis name)
  "Checks the given axis is a valid 3-item list of two strings and a number - throws error if invalid"
  (or (and (listp axis)
           (= 3 (length axis))
           (stringp (first axis))
           (stringp (second axis))
           (numberp (third axis)))
      (error "Given axis ~a is not a valid time axis for ~a" axis name)))

