;; LTk wrapper around the tklib plotchart
;;
;; -- order of definition and naming follows that in the plotchart documentation
;; https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/plotchart/plotchart.html
;;
;; Not implemented (because ...):
;;
;; - chart-saveplot - option for :format requires Img library, so not supported
;; - xconfig / yconfig - unclear how much needed beyond :format
;;   "The commands xconfig and yconfig are currently implemented only for
;;   XY-plots and only the option -format has any effect."
;; - $anyplot object itemtype series args
;; - $anyplot deletedata
;; - normal-plot - requires math::statistics package

(in-package :ltk-plotchart)

(eval-when (:load-toplevel)
  (setf *init-wish-hook* (append *init-wish-hook*
                                 (list (lambda ()
                                   (send-wish "package require Plotchart"))))))

;; ---------------------------------------------------------------------------
;; Plotchart - parent class and common functions

(defvar *plot-count* 0)

(defclass plotchart ()
  ((name :accessor name :initform "plotchart")
   (canvas :accessor canvas :initarg :canvas :initform nil)))

(defmethod initialize-instance :before ((chart plotchart) &key)
  (setf (name chart) (format nil "plotchart_~d" *plot-count*))
  (incf *plot-count*))

(defgeneric chart-title (chart title &optional position))
(defmethod chart-title ((chart plotchart) title &optional (position :center))
  "Sets the chart title: position can be :center, :left or :right"
  (format-wish "$~a title \"~a\" \"~a\"" (name chart) title (string-downcase (string position))))

(defgeneric chart-subtitle (chart title))
(defmethod chart-subtitle ((chart plotchart) title)
  "Sets the chart subtitle"
  (format-wish "$~a subtitle \"~a\"" (name chart) title))

(defgeneric chart-saveplot (chart filename &key plotregion))
(defmethod chart-saveplot ((chart plotchart) filename &key (plotregion :window))
  "Saves the chart to a postscript file"
  (format-wish "$~a saveplot \"~a\" -plotregion ~a" 
               (name chart) 
               filename 
               (string-downcase (string plotregion))))

(defgeneric chart-xtext (chart xtext))
(defmethod chart-xtext ((chart plotchart) xtext)
  "Sets the title of the (horizonal) x-axis"
  (format-wish "$~a xtext \"~a\"" (name chart) xtext))

(defgeneric chart-ytext (chart ytext))
(defmethod chart-ytext ((chart plotchart) ytext)
  "Sets the title of the (vertical) y-axis"
  (format-wish "$~a ytext \"~a\"" (name chart) ytext))

(defgeneric chart-vtext (chart ytext))
(defmethod chart-vtext ((chart plotchart) vtext)
  "Sets the title of the (vertical) y-axis, and displays vertically along axis"
  (format-wish "$~a vtext \"~a\"" (name chart) vtext))

(defgeneric chart-xsubtext (chart text))
(defmethod chart-xsubtext ((chart plotchart) text)
  "Sets the subtext of the (horizontal) x-axis"
  (format-wish "$~a xsubtext \"~a\"" (name chart) text))

(defgeneric chart-ysubtext (chart text))
(defmethod chart-ysubtext ((chart plotchart) text)
  "Sets the subtext of the (vertical) y-axis"
  (format-wish "$~a ysubtext \"~a\"" (name chart) text))

(defgeneric chart-vsubtext (chart text))
(defmethod chart-vsubtext ((chart plotchart) text)
  "Sets the subtext of the (vertical) y-axis, and displays vertically along axis"
  (format-wish "$~a vsubtext \"~a\"" (name chart) text))

(defgeneric chart-background (chart part colour-or-image direction &optional brightness))
(defmethod chart-background ((chart plotchart) part colour-or-image direction &optional (brightness :bright))
  "Sets the background of a part of the plot"
  (format-wish "$~a background \"~a\" \"~a\" \"~a\" \"~a\""
               (name chart) part colour-or-image direction
               (string-downcase (string brightness))))

(defgeneric chart-xticklines (chart &optional colour dash))
(defmethod chart-xticklines ((chart plotchart) &optional (colour "black") (dash :lines))
  "Draw vertical ticklines at each tick location"
  (format-wish "$~a xticklines \"~a\" \"~a\""
               (name chart) colour (string-downcase (string dash))))

(defgeneric chart-yticklines (chart colour &optional dash))
(defmethod chart-yticklines ((chart plotchart) colour &optional (dash :lines))
  "Draw horizontal ticklines at each tick location"
  (format-wish "$~a yticklines \"~a\" \"~a\""
               (name chart) colour (string-downcase (string dash))))

(defgeneric chart-legend (chart series text &optional spacing))
(defmethod chart-legend ((chart plotchart) series text &optional spacing)
  (format-wish "$~a legend \"~a\" \"~a\" ~a" (name chart) series text (if spacing spacing "")))

(defgeneric chart-removefromlegend (chart series))
(defmethod chart-removefromlegend ((chart plotchart) series)
  "Removes series from legend and redraws it"
  (format-wish "$~a removefromlegend \"~a\"" (name chart) series))

(defgeneric chart-legendconfig (chart &key background border canvas font legendtype position spacing))
(defmethod chart-legendconfig ((chart plotchart) &key background border canvas font legendtype position spacing)
  (let ((option-string ""))
    (when background
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-background "
                                       (string-downcase (string background)))))
    (when border
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-border "
                                       (string-downcase (string border)))))
    (when canvas
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-canvas "
                                       (widget-path canvas))))
    (when font
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-font "
                                       (string-downcase (string font)))))
    (when legendtype
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-legendtype "
                                       (string-downcase (string legendtype)))))
    (when position
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-position "
                                       (string-downcase (string position)))))
    (when spacing
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-spacing "
                                       (string spacing))))
    (format-wish "$~a legendconfig ~a" (name chart) option-string)))

(defgeneric chart-balloon (chart x y text direction))
(defmethod chart-balloon ((chart plotchart) x y text direction)
  "Adds balloon text to the plot, with pointer to given coordinates"
  (format-wish "$~a balloon ~f ~f \"~a\" \"~a\"" 
               (name chart) x y text (string-downcase (string direction))))

(defgeneric chart-balloonconfig (chart &key font justify textcolour textcolor 
                                       background outline margin rimwidth arrowsize))
(defmethod chart-balloonconfig ((chart plotchart) &key font justify textcolour 
                                                  textcolor background outline margin 
                                                  rimwidth arrowsize)
  "Configures balloon text for given plot - settings apply to next call to chart-balloon"
  (let ((option-string ""))
    (when font
      (setf option-string (format nil "~a -font \"~a\"" option-string font)))
    (when justify
      (setf option-string (format nil "~a -justify ~a"
                                  option-string
                                  (string-downcase (string justify)))))
    (when textcolour
      (setf option-string (format nil "~a -textcolour ~a"
                                  option-string
                                  (string-downcase (string textcolour)))))
    (when textcolor
      (setf option-string (format nil "~a -textcolour ~a"
                                  option-string
                                  (string-downcase (string textcolor)))))
    (when background
      (setf option-string (format nil "~a -background ~a"
                                  option-string
                                  (string-downcase (string background)))))
    (when outline
      (setf option-string (format nil "~a -outline ~a"
                                  option-string
                                  (string-downcase (string outline)))))
    (when margin
      (setf option-string (format nil "~a -margin ~d" option-string margin)))
    (when rimwidth
      (setf option-string (format nil "~a -rimwidth ~d" option-string rimwidth)))
    (when arrowsize
      (setf option-string (format nil "~a -arrowsize ~d" option-string arrowsize)))
    (format-wish "$~a balloonconfig ~a" (name chart) option-string)))

(defgeneric chart-plaintext (chart x y text &optional direction))
(defmethod chart-plaintext ((chart plotchart) x y text &optional (direction "n"))
  "Adds plaintext to the plot, at given coordinates"
  (format-wish "$~a plaintext ~f ~f \"~a\" \"~a\"" 
               (name chart) x y text (string-downcase (string direction))))

(defgeneric chart-plaintextconfig (chart &key font justify textcolour textcolor)) 
(defmethod chart-plaintextconfig ((chart plotchart) &key font justify textcolour textcolor)
  "Configures plaintext for given plot - settings apply to next call to chart-balloon"
  (let ((option-string ""))
    (when font
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-font "
                                       (string-downcase (string font)))))
    (when justify
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-justify "
                                       (string-downcase (string justify)))))
    (when textcolour
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-textcolour "
                                       (string-downcase (string textcolour)))))
    (when textcolor
      (setf option-string (uiop:strcat option-string
                                       " "
                                       "-textcolour "
                                       (string-downcase (string textcolor)))))
    (format-wish "$~a balloonconfig ~a" (name chart) option-string)))

;; ---------------------------------------------------------------------------
;; XY Plot

(defclass xy-plot (plotchart) 
  ((xaxis :accessor xaxis :initarg :xaxis :initform '(0 10 1))
   (yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))))

(defmethod initialize-instance :after ((chart xy-plot) &key xlabels ylabels box
                                                       axesbox timeformat gmt
                                                       axestozero isometric)
  (format-wish "set ~a [::Plotchart::createXYPlot ~a {~{ ~d~} } {~{ ~d~} }~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (xaxis chart)
               (yaxis chart)
               (make-xy-args xlabels ylabels box axesbox timeformat gmt axestozero isometric)
               ))

(defun make-xy-args (xlabels ylabels box axesbox timeformat gmt &optional axestozero isometric)
  (let ((result ""))
    (when xlabels
      (setf result (format nil "~a -xlabels {~{ ~a~} }" result xlabels)))
    (when ylabels
      (setf result (format nil "~a -ylabels {~{ ~a~} }" result ylabels)))
    (when box
      (setf result (format nil "~a -box {~{ ~a~} }" result box)))
    (when axesbox
      (setf result (format nil "~a -axesbox {~{ ~a~} }" result axesbox)))
    (when timeformat
      (setf result (format nil "~a -timeformat \"~a\"" result timeformat)))
    (when gmt
      (setf result (format nil "~a -gmt ~d" result gmt)))
    (when axestozero
      (setf result (format nil "~a -axestozero ~d" result axestozero)))
    (when isometric
      (setf result (format nil "~a -isometric ~d" result isometric)))
    result))

(defgeneric xy-plot-plot (chart series x-coord y-coord))
(defmethod xy-plot-plot ((chart xy-plot) series x-coord y-coord)
  "Adds a data point to the chart"
  (format-wish "$~a plot ~a ~f ~f" (name chart) series x-coord y-coord))

(defgeneric xy-plot-plotlist (chart series xlist ylist &optional every))
(defmethod xy-plot-plotlist ((chart xy-plot) series xlist ylist &optional every)
  "Draws a series of data as a whole"
  (format-wish "$~a plotlist ~a {~{ ~$~} } {~{ ~$~} } ~a"
               (name chart)
               series
               xlist
               ylist
               (if every every "")))

(defgeneric xy-plot-region (chart series xlist ylist))
(defmethod xy-plot-region ((chart xy-plot) series xlist ylist)
  "Draws a filled polygon"
  (format-wish "$~a region ~a {~{ ~$~} } {~{ ~$~} }"
               (name chart)
               series
               xlist
               ylist))

(defgeneric xy-plot-minmax (chart series xcoord ymin ymax))
(defmethod xy-plot-minmax ((chart xy-plot) series xcoord ymin ymax)
  "Draws a filled strip representing a minimum and maximum"
  (format-wish "$~a minmax ~a ~f ~f ~f"
               (name chart) series xcoord ymin ymax))

(defgeneric xy-plot-dataconfig (chart series &key colour color type symbol
                                      radius width filled fillcolour style))
(defmethod xy-plot-dataconfig ((chart xy-plot) series &key colour color type
                                               symbol radius width filled
                                               fillcolour style)
  (format-wish "$~a dataconfig ~a ~a" 
               (name chart) 
               series 
               (make-config-args colour color type symbol radius width filled
                                 fillcolour style)))

(defun make-config-args (colour color type symbol radius width filled
                                fillcolour style &optional smooth)
  (let ((result ""))
    (when color
      (setf result (format nil "~a -colour ~a" result color)))
    (when colour
      (setf result (format nil "~a -colour ~a" result colour)))
    (when type
      (setf result (format nil "~a -type ~a" result (string-downcase (string type)))))
    (when symbol
      (setf result (format nil "~a -symbol ~a" result (string-downcase (string symbol)))))
    (when width
      (setf result (format nil "~a -width ~d" result width)))
    (when filled
      (setf result (format nil "~a -filled ~a" result (string-downcase (string filled)))))
    (when fillcolour
      (setf result (format nil "~a -fillcolour ~a" result fillcolour)))
    (when style
      (setf result (format nil "~a -style ~a" result (string-downcase (string style)))))
    (when smooth
      (setf result (format nil "~a -smooth ~a" result smooth)))
    result))

;; ---------------------------------------------------------------------------
;; Strip Chart

(defclass strip-chart (plotchart)
  ())

(defgeneric strip-chart-plot (chart series x-coord y-coord))
(defmethod strip-chart-plot ((chart strip-chart) series x-coord y-coord)
  "Adds a data point to the chart"
  (format-wish "$~a plot ~a ~f ~f" (name chart) series x-coord y-coord))

(defgeneric strip-chart-dataconfig (chart series &key colour color type symbol
                                          radius width filled fillcolour style))
(defmethod strip-chart-dataconfig ((chart strip-chart) series &key colour color type
                                                       symbol radius width filled
                                                       fillcolour style)
  (format-wish "$~a dataconfig ~a ~a" 
               (name chart) 
               series 
               (make-config-args colour color type symbol radius width filled
                                 fillcolour style)))

;; ---------------------------------------------------------------------------
;; TX Plot

(defclass tx-plot (plotchart)
  ((timeaxis :accessor timeaxis :initarg :timeaxis :initform '("2021-01-01" "2021-01-31" 1))
   (xaxis :accessor xaxis :initarg :xaxis :initform '(0 10 1))))

(defmethod initialize-instance :after ((chart tx-plot) &key xlabels ylabels box
                                                       axesbox timeformat gmt
                                                       axestozero isometric)
  (format-wish "set ~a [::Plotchart::createTXPlot ~a {~{ ~a~} } {~{ ~d~} }~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (timeaxis chart)
               (xaxis chart)
               (make-xy-args xlabels ylabels box axesbox timeformat gmt axestozero isometric)
               ))

(defgeneric tx-plot-plot (chart series time-coord x-coord))
(defmethod tx-plot-plot ((chart tx-plot) series time-coord x-coord)
  "Adds a data point to the chart"
  (format-wish "$~a plot ~a ~a ~f" (name chart) series time-coord x-coord))

(defgeneric tx-plot-dataconfig (chart series &key colour color type symbol
                                      radius width filled fillcolour style))
(defmethod tx-plot-dataconfig ((chart tx-plot) series &key colour color type
                                               symbol radius width filled
                                               fillcolour style)
  (format-wish "$~a dataconfig ~a ~a" 
               (name chart) 
               series 
               (make-config-args colour color type symbol radius width filled
                                 fillcolour style)))

;; ---------------------------------------------------------------------------
;; X-LogY Plot
;; - XY Plot with y axis having logarithmic values

(defclass x-logy-plot (xy-plot)
  ())

;; ---------------------------------------------------------------------------
;; LogX-Y Plot
;; - XY Plot with x axis having logarithmic values

(defclass logx-y-plot (xy-plot)
  ())

;; ---------------------------------------------------------------------------
;; LogX-LogY Plot
;; - XY Plot with both axes having logarithmic values

(defclass logx-logy-plot (xy-plot)
  ())

;; ---------------------------------------------------------------------------
;; Polar Plot

(defclass polar-plot (plotchart)
  ())

(defgeneric polar-plot-dataconfig (chart series &key colour color type symbol
                                         radius width filled fillcolour style))
(defmethod polar-plot-dataconfig ((chart polar-plot) series &key colour color type
                                                     symbol radius width filled
                                                     fillcolour style)
  (format-wish "$~a dataconfig ~a ~a" 
               (name chart) 
               series 
               (make-config-args colour color type symbol radius width filled
                                 fillcolour style)))

;; ---------------------------------------------------------------------------
;; Windrose

(defclass windrose (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Isometric Plot

(defclass isometric-plot (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Histogram 

(defclass histogram (plotchart)
  ((xaxis :accessor xaxis :initarg :xaxis :initform '(0 10 1))
   (yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))))

(defmethod initialize-instance :after ((chart histogram) &key xlabels ylabels box
                                                         axesbox timeformat gmt)
  (format-wish "set ~a [::Plotchart::createHistogram ~a {~{ ~d~} } {~{ ~d~} }~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (xaxis chart)
               (yaxis chart)
               (make-xy-args xlabels ylabels box axesbox timeformat gmt)
               ))

(defgeneric histogram-plot (chart series x-coord y-coord))
(defmethod histogram-plot ((chart histogram) series x-coord y-coord)
  "Adds a data point to the chart"
  (format-wish "$~a plot ~a ~f ~f" (name chart) series x-coord y-coord))

(defgeneric histogram-plotcumulative (chart series x-coord y-coord))
(defmethod histogram-plotcumulative ((chart histogram) series x-coord y-coord)
  "Adds a data point to the chart, accumulating previous points"
  (format-wish "$~a plotcumulative ~a ~f ~f" (name chart) series x-coord y-coord))

(defgeneric histogram-dataconfig (chart series &key colour color type symbol
                                        radius width filled fillcolour style))
(defmethod histogram-dataconfig ((chart histogram) series &key colour color type
                                                   symbol radius width filled
                                                   fillcolour style)
  (format-wish "$~a dataconfig ~a ~a" 
               (name chart) 
               series 
               (make-config-args colour color type symbol radius width filled
                                 fillcolour style)))

;; ---------------------------------------------------------------------------
;; 3D Plot

(defclass threed-plot (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; 3D Ribbon Plot

(defclass threed-ribbon-plot (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Pie Chart

(defclass pie-chart (plotchart)
  ())

(defmethod initialize-instance :after ((chart pie-chart) &key)
  (format-wish "set ~a [::Plotchart::createPiechart ~a]" (name chart) (widget-path (canvas chart))))

(defun item2string (item)
  (typecase item
    (string
      (format nil "\"~a\"" item))
    (otherwise
      (format nil "~d" item))))

(defun data2string (data)
  "convert dotted list to {\"item\" n \"item\" n} format"
  (let ((result ""))
    (dolist (pair data)
      (setf result (uiop:strcat result
                                " " 
                                (item2string (car pair)) 
                                " "
                                (item2string (cdr pair)))))
    result))

(defgeneric pie-chart-plot (chart data))
(defmethod pie-chart-plot ((chart pie-chart) data)
  (format-wish "$~a plot {~a}" (name chart) (data2string data)))

;; ---------------------------------------------------------------------------
;; Spiral Pie 

(defclass spiral-pie (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Radial Chart

(defclass radial-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Bar Chart

(defclass bar-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Horizontal Bar Chart

(defclass horizontal-bar-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; 3D Bar Chart

(defclass threed-bar-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; 3D Ribbon Chart

(defclass threed-ribbon-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Box Plot

(defclass box-plot (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Time Chart

(defclass time-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Gantt Chart

(defclass gantt-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Right Axis

(defclass right-axis (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Table Chart

(defclass table-chart (plotchart)
  ())

;; ---------------------------------------------------------------------------
;; Ternary Diagram

(defclass ternary-diagram (plotchart)
  ())

(defgeneric ternary-diagram-dataconfig (chart series &key colour color type symbol
                                              radius width filled fillcolour style))
(defmethod ternary-diagram-dataconfig ((chart ternary-diagram) series &key colour color type
                                                               symbol radius width filled
                                                               fillcolour style smooth)
  (format-wish "$~a dataconfig ~a ~a" 
               (name chart) 
               series 
               (make-config-args colour color type symbol radius width filled
                                 fillcolour style smooth)))

;; ---------------------------------------------------------------------------
;; Status timeline

(defclass status-timeline (plotchart)
  ())


