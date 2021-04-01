;; LTk wrapper around the tklib plotchart module
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
;; - 3dplots - plotfunc, plotfuncont - not sure how to handle function (callback?)
;; - $table formatcommand procname - not sure how to handle procedures
;; - status-timeline-vertline args for line definition - no description given in documentation

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

(defgeneric xy-plot-trend (chart series xcoord ycoord))
(defmethod xy-plot-trend ((chart xy-plot) series xcoord ycoord)
  "Draws or updates a trend line, given data so far"
  (format-wish "$~a trend ~a ~f ~f"
               (name chart) series xcoord ycoord))

(defgeneric xy-plot-rchart (chart series xcoord ycoord))
(defmethod xy-plot-rchart ((chart xy-plot) series xcoord ycoord)
  "Like plot, but adds +/- s.d. line"
  (format-wish "$~a rchart ~a ~f ~f"
               (name chart) series xcoord ycoord))

(defgeneric xy-plot-interval (chart series xcoord ymin ymax &optional ycenter))
(defmethod xy-plot-interval ((chart xy-plot) series xcoord ymin ymax &optional ycenter)
  "Adds a vertical error interval"
  (format-wish "$~a interval ~a ~f ~f ~f ~a"
               (name chart) series xcoord ymin ymax
               (if ycenter ycenter "")))

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
      (setf result (format nil "~a -colour ~a" result (string-downcase (string color)))))
    (when colour
      (setf result (format nil "~a -colour ~a" result (string-downcase (string colour)))))
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

;; $xyplot box-and-whiskers series xcrd ycrd
;; $xyplot vector series xcrd ycrd ucmp vcmp
;; $xyplot vectorconfig series -option value ...
;; $xyplot dot series xcrd ycrd value
;; $xyplot dotconfig series -option value ...
;; $xyplot contourlines xcrd ycrd values ?classes?
;; $xyplot contourlinesfunctionvalues xvec yvec valuesmat ?classes?
;; $xyplot contourfill xcrd ycrd values ?classes?
;; $xyplot contourbox xcrd ycrd values ?classes?
;; $xyplot colorMap colours
;; $xyplot legendisolines values classes
;; $xyplot legendshades values classes
;; $xyplot grid xcrd ycrd
;; $xyplot xband ymin ymax
;; $xyplot yband xmin xmax
;; $xyplot labeldot x y text orient

;; ---------------------------------------------------------------------------
;; Strip Chart

(defclass strip-chart (xy-plot)
  ())

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
  ((radius-data :accessor radius-data :initarg :radius-data :initform '(10 1))))

(defmethod initialize-instance :after ((chart polar-plot) &key xlabels ylabels box
                                                          axesbox timeformat gmt
                                                          axestozero isometric)
  (format-wish "set ~a [::Plotchart::createPolarplot ~a {~{ ~d~} }~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (radius-data chart)
               (make-xy-args xlabels ylabels box axesbox timeformat gmt axestozero isometric)
               ))

(defgeneric polar-plot-plot (chart series radius angle))
(defmethod polar-plot-plot ((chart polar-plot) series radius angle)
  "Adds a data point to the chart"
  (format-wish "$~a plot ~a ~f ~f" (name chart) series radius angle))

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
  ((radius-data :accessor radius-data :initarg :radius-data :initform '(10 1))
   (sectors :accessor sectors :initarg :sectors :initform 16)))

(defmethod initialize-instance :after ((chart windrose) &key)
  (format-wish "set ~a [::Plotchart::createWindrose ~a {~{ ~d~} } ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (radius-data chart)
               (sectors chart)
               ))

(defgeneric windrose-plot (chart data colour))
(defmethod windrose-plot ((chart windrose) data colour)
  "Draw the given data into the existing spokes"
  (format-wish "$~a plot {~{ ~d~} } ~a" (name chart) data colour))

;; ---------------------------------------------------------------------------
;; Isometric Plot

(defclass isometric-plot (plotchart)
  ((xaxis :accessor xaxis :initarg :xaxis :initform '(0 10))
   (yaxis :accessor yaxis :initarg :yaxis :initform '(0 10))
   (stepsize :accessor stepsize :initarg :stepsize :initform :noaxes)))

(defmethod initialize-instance :after ((chart isometric-plot) &key)
  (format-wish "set ~a [::Plotchart::createHistogram ~a {~{ ~d~} } {~{ ~d~} } ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (xaxis chart)
               (yaxis chart)
               (if (numberp (stepsize chart))
                 (format nil "~f" (stepsize chart))
                 (string-downcase (string (stepsize chart))))
               ))

(defgeneric isometric-plot-rectangle (chart x1 y1 x2 y2 colour))
(defmethod isometric-plot-rectangle ((chart isometric-plot) x1 y1 x2 y2 colour)
  "Draw the outlines of specified rectangle"
  (format-wish "$~a plot rectangle ~f ~f ~f ~f ~a" (name chart) x1 y1 x2 y2 colour))

(defgeneric isometric-plot-filled-rectangle (chart x1 y1 x2 y2 colour))
(defmethod isometric-plot-filled-rectangle ((chart isometric-plot) x1 y1 x2 y2 colour)
  "Draw and fill specified rectangle"
  (format-wish "$~a plot filled-rectangle ~f ~f ~f ~f ~a" (name chart) x1 y1 x2 y2 colour))

(defgeneric isometric-plot-circle (chart xc yc radius colour))
(defmethod isometric-plot-circle ((chart isometric-plot) xc yc radius colour)
  "Draw the outlines of specified circle"
  (format-wish "$~a plot circle ~f ~f ~f ~a" (name chart) xc yc radius colour))

(defgeneric isometric-plot-filled-circle (chart xc yc radius colour))
(defmethod isometric-plot-filled-circle ((chart isometric-plot) xc yc radius colour)
  "Draw and fill specified circle"
  (format-wish "$~a plot filled-circle ~f ~f ~f ~a" (name chart) xc yc radius colour))

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
  ((xaxis :accessor xaxis :initarg :xaxis :initform '(0 10 1))
   (yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))
   (zaxis :accessor zaxis :initarg :zaxis :initform '(0 10 1))))

(defmethod initialize-instance :after ((chart threed-plot) &key xlabels)
  (format-wish "set ~a [::Plotchart::create3DPlot ~a {~{ ~d~} } {~{ ~d~} } {~{ ~d~} } ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (xaxis chart)
               (yaxis chart)
               (zaxis chart)
               (if xlabels (format nil "-xlabels {~{ ~a~} }" xlabels) "")
               ))

(defgeneric threed-plot-gridsize (chart nxcells nycells))
(defmethod threed-plot-gridsize ((chart threed-plot) nxcells nycells)
  "Sets the grid resolution in the 3d plot"
  (format-wish "$~a gridsize ~d ~d" (name chart) nxcells nycells))

(defgeneric threed-plot-plotdata (chart data))
(defmethod threed-plot-plotdata ((chart threed-plot) data)
  "Plots given list-of-lists data"
  (format-wish "$~a plotdata {~&~a~&}"
               (name chart)
               (apply #'uiop:strcat 
                      (mapcar #'(lambda (row) (format nil "{~{ ~f~} }~&" row)) data))))

(defgeneric threed-plot-interpolatedata (chart data contours))
(defmethod threed-plot-interpolatedata ((chart threed-plot) data contours)
  "Plots given list-of-lists data with interpolated contours"
  (format-wish "$~a interpolatedata {~&~a~&} {~{ ~f~} }"
               (name chart)
               (apply #'uiop:strcat 
                      (mapcar #'(lambda (row) (format nil "{~{ ~f~} }~&" row)) data))
               contours))

(defgeneric threed-plot-colour (chart fill border))
(defmethod threed-plot-colour ((chart threed-plot) fill border)
  "Sets the fill and border colour"
  (format-wish "$~a colour ~a ~a" 
               (name chart) 
               (string-downcase (string fill)) 
               (string-downcase (string border))))

(defgeneric threed-plot-color (chart fill border))
(defmethod threed-plot-color ((chart threed-plot) fill border)
  "Sets the fill and border colour"
  (format-wish "$~a colour ~a ~a" 
               (name chart) 
               (string-downcase (string fill)) 
               (string-downcase (string border))))

(defgeneric threed-plot-ribbon (chart yzpairs))
(defmethod threed-plot-ribbon ((chart threed-plot) yzpairs)
  "Plots a ribbon based on given yz-pairs"
  (format-wish "$~a ribbon {~&~a~&}"
               (name chart)
               (apply #'uiop:strcat 
                      (mapcar #'(lambda (row) (format nil "{~{ ~f~} }~&" row)) yzpairs))))

;; ---------------------------------------------------------------------------
;; 3D Ribbon Plot

(defclass threed-ribbon-plot (plotchart)
  ((yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))
   (zaxis :accessor zaxis :initarg :zaxis :initform '(0 10 1))))

(defmethod initialize-instance :after ((chart threed-ribbon-plot) &key)
  (format-wish "set ~a [::Plotchart::create3DRibbonPlot ~a {~{ ~d~} } {~{ ~d~} }]" 
               (name chart) 
               (widget-path (canvas chart))
               (yaxis chart)
               (zaxis chart)
               ))

(defgeneric threed-ribbon-plot-colour (chart fill border))
(defmethod threed-ribbon-plot-colour ((chart threed-ribbon-plot) fill border)
  "Sets the fill and border colour"
  (format-wish "$~a colour ~a ~a" 
               (name chart) 
               (string-downcase (string fill)) 
               (string-downcase (string border))))

(defgeneric threed-ribbon-plot-color (chart fill border))
(defmethod threed-ribbon-plot-color ((chart threed-ribbon-plot) fill border)
  "Sets the fill and border colour"
  (format-wish "$~a colour ~a ~a" 
               (name chart) 
               (string-downcase (string fill)) 
               (string-downcase (string border))))

(defgeneric threed-ribbon-plot-plot (chart yzpairs))
(defmethod threed-ribbon-plot-plot ((chart threed-ribbon-plot) yzpairs)
  "Plots a ribbon based on given yz-pairs"
  (format-wish "$~a plot {~&~a~&}"
               (name chart)
               (apply #'uiop:strcat 
                      (mapcar #'(lambda (row) (format nil "{~{ ~f~} }~&" row)) yzpairs))))

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

(defgeneric pie-chart-colors (chart &rest colours))
(defmethod pie-chart-colors ((chart pie-chart) &rest colours)
  "Sets the colours for the pie slices"
  (format-wish "$~a colours ~{~a ~}" 
               (name chart)
               (mapcar #'(lambda (c) (string-downcase (string c))) colours)))

(defgeneric pie-chart-colours (chart &rest colours))
(defmethod pie-chart-colours ((chart pie-chart) &rest colours)
  "Sets the colours for the pie slices"
  (format-wish "$~a colours ~{~a ~}" 
               (name chart)
               (mapcar #'(lambda (c) (string-downcase (string c))) colours)))

(defgeneric pie-chart-explode (chart segment))
(defmethod pie-chart-explode ((chart pie-chart) segment)
  "Displays segment number out of the circle"
  (format-wish "$~a explode ~d" (name chart) segment))

;; ---------------------------------------------------------------------------
;; Spiral Pie 

(defclass spiral-pie (pie-chart) 
  ())

;; ---------------------------------------------------------------------------
;; Radial Chart

(defclass radial-chart (plotchart)
  ((names :accessor names :initarg :names :initform nil)
   (scale :accessor radial-chart-scale :initarg :scale :initform 1.0)
   (style :accessor style :initarg :style :initform "lines")))

(defmethod initialize-instance :after ((chart radial-chart) &key)
  (format-wish "set ~a [::Plotchart::createRadialchart {~{ ~a~} } ~f ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (names chart)
               (scale chart)
               (string-downcase (string (style chart)))))

(defgeneric radial-chart-plot (chart data colour &optional thickness))
(defmethod radial-chart-plot ((chart radial-chart) data colour &optional thickness)
  (format-wish "$~a plot { ~{ ~a~} } ~a ~a" 
               (name chart) 
               data 
               (string-downcase (string colour)) 
               (if thickness thickness "")))

(defgeneric radial-chart-colors (chart &rest colours))
(defmethod radial-chart-colors ((chart radial-chart) &rest colours)
  "Sets the colours for the radial spokes"
  (format-wish "$~a colours ~{~a ~}" 
               (name chart)
               (mapcar #'(lambda (c) (string-downcase (string c))) colours)))

(defgeneric radial-chart-colours (chart &rest colours))
(defmethod radial-chart-colours ((chart radial-chart) &rest colours)
  "Sets the colours for the pie spokes"
  (format-wish "$~a colours ~{~a ~}" 
               (name chart)
               (mapcar #'(lambda (c) (string-downcase (string c))) colours)))

;; ---------------------------------------------------------------------------
;; Bar Chart

(defclass bar-chart (plotchart)
  ((xlabels :accessor xlabels :initarg :xlabels :initform nil)
   (yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))
   (noseries :accessor noseries :initarg :noseries :initform 1)))

(defmethod initialize-instance :after ((chart bar-chart) &key xlabelangle)
  (format-wish "set ~a [::Plotchart::createBarchart ~a {~{ ~a~} } {~{ ~d~} } ~a~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (xlabels chart)
               (yaxis chart)
               (if (numberp (noseries chart)) 
                 (noseries chart)
                 (string-downcase (string (noseries chart))))
               (if xlabelangle (format nil " -xlabelangle ~d" xlabelangle) "")
               ))

(defgeneric bar-chart-plot (chart series ydata colour &optional direction brightness))
(defmethod bar-chart-plot ((chart bar-chart) series ydata colour &optional direction brightness)
  (format-wish "$~a plot ~a { ~{ ~a~} } ~a ~a ~a" 
               (name chart) 
               series
               ydata 
               (string-downcase (string colour))
               (if direction (string-downcase (string direction)) "")
               (if brightness (string-downcase (string brightness)) "")))

(defgeneric bar-chart-config (chart &key showvalues valuefont valuecolour valueformat))
(defmethod bar-chart-config ((chart bar-chart) &key showvalues valuefont valuecolour valueformat)
  (when showvalues
    (format-wish "$~a config -showvalues ~a" (name chart) showvalues))
  (when valuefont
    (format-wish "$~a config -valuefont \"~a\"" (name chart) valuefont))
  (when valuecolour
    (format-wish "$~a config -valuecolour ~a" (name chart) (string-downcase (string valuecolour))))
  (when valueformat
    (format-wish "$~a config -valueformat \"~a\"" (name chart) valueformat)))

;; ---------------------------------------------------------------------------
;; Horizontal Bar Chart

(defclass horizontal-bar-chart (plotchart)
  ((xaxis :accessor xaxis :initarg :xaxis :initform '(0 10 1))
   (ylabels :accessor ylabels :initarg :ylabels :initform nil)
   (noseries :accessor noseries :initarg :noseries :initform 1)))

(defmethod initialize-instance :after ((chart horizontal-bar-chart) &key)
  (format-wish "set ~a [::Plotchart::createHorizontalBarchart ~a {~{ ~d~} } {~{ ~a~} } ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (xaxis chart)
               (ylabels chart)
               (if (numberp (noseries chart))
                 (noseries chart)
                 (string-downcase (string (noseries chart))))))

(defgeneric horizontal-bar-chart-plot (chart series xdata colour &optional direction brightness))
(defmethod horizontal-bar-chart-plot ((chart horizontal-bar-chart) series xdata colour 
                                                                   &optional direction brightness)
  (format-wish "$~a plot ~a { ~{ ~a~} } ~a ~a ~a" 
               (name chart) 
               series
               xdata 
               (string-downcase (string colour))
               (if direction (string-downcase (string direction)) "")
               (if brightness (string-downcase (string brightness)) "")))

(defgeneric horizontal-bar-chart-config (chart &key showvalues valuefont
                                               valuecolour valueformat))
(defmethod horizontal-bar-chart-config ((chart horizontal-bar-chart) &key
                                                                     showvalues
                                                                     valuefont
                                                                     valuecolour
                                                                     valueformat)
  (when showvalues
    (format-wish "$~a config -showvalues ~a" (name chart) showvalues))
  (when valuefont
    (format-wish "$~a config -valuefont \"~a\"" (name chart) valuefont))
  (when valuecolour
    (format-wish "$~a config -valuecolour ~a" (name chart) (string-downcase (string valuecolour))))
  (when valueformat
    (format-wish "$~a config -valueformat \"~a\"" (name chart) valueformat)))

;; ---------------------------------------------------------------------------
;; 3D Bar Chart

(defclass threed-bar-chart (plotchart)
  ((yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))
   (nobars :accessor nobars :initarg :nbars :initform 1)))

(defmethod initialize-instance :after ((chart threed-bar-chart) &key)
  (format-wish "set ~a [::Plotchart::create3DBarchart ~a {~{ ~d~} } ~d]" 
               (name chart) 
               (widget-path (canvas chart))
               (yaxis chart)
               (nobars chart)
               ))

(defgeneric threed-bar-chart-plot (chart label yvalue colour))
(defmethod threed-bar-chart-plot ((chart threed-bar-chart) label yvalue colour)
  (format-wish "$~a plot ~a ~f ~a" 
               (name chart) 
               label
               yvalue
               (string-downcase (string colour))))

(defgeneric threed-bar-chart-config (chart 
                                      &key usebackground useticklines
                                      showvalues labelfont labelcolour
                                      valuefont valuecolour))
(defmethod threed-bar-chart-config ((chart threed-bar-chart) 
                                    &key usebackground useticklines showvalues
                                    labelfont labelcolour valuefont valuecolour)
  (when usebackground
    (format-wish "$~a config -usebackground ~a" (name chart) usebackground))
  (when useticklines
    (format-wish "$~a config -useticklines ~a" (name chart) useticklines))
  (when showvalues
    (format-wish "$~a config -showvalues ~a" (name chart) showvalues))
  (when labelfont
    (format-wish "$~a config -labelfont \"~a\"" (name chart) labelfont))
  (when labelcolour
    (format-wish "$~a config -labelcolour ~a" (name chart) (string-downcase (string labelcolour))))
  (when valuefont
    (format-wish "$~a config -valuefont \"~a\"" (name chart) valuefont))
  (when valuecolour
    (format-wish "$~a config -valuecolour ~a" (name chart) (string-downcase (string valuecolour)))))

;; ---------------------------------------------------------------------------
;; 3D Ribbon Chart

(defclass threed-ribbon-chart (plotchart)
  ((names :accessor names :initarg :names :initform nil)
   (yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))
   (zaxis :accessor zaxis :initarg :zaxis :initform '(0 10 1))
   (nobars :accessor nobars :initarg :nbars :initform 1)))

(defmethod initialize-instance :after ((chart threed-ribbon-chart) &key)
  (format-wish "set ~a [::Plotchart::create3DRibbonChart ~a {~{ ~a~} } {~{ ~d~} } {~{ ~d~} } ~d]" 
               (name chart) 
               (widget-path (canvas chart))
               (names chart)
               (yaxis chart)
               (zaxis chart)
               (nobars chart)
               ))

(defgeneric threed-ribbon-chart-line (chart xypairs colour))
(defmethod threed-ribbon-chart-line ((chart threed-ribbon-chart) xypairs colour)
  "Plots a 3D ribbon based on given xy-pairs"
  (format-wish "$~a line {~&~a~&} ~a"
               (name chart)
               (apply #'uiop:strcat 
                      (mapcar #'(lambda (row) (format nil "{~{ ~f~} }~&" row)) xypairs))
               (string-downcase (string colour))))

(defgeneric threed-ribbon-chart-area (chart xypairs colour))
(defmethod threed-ribbon-chart-area ((chart threed-ribbon-chart) xypairs colour)
  "Plots a 3D ribbon based on given xy-pairs with filled area in front"
  (format-wish "$~a area {~&~a~&} ~a"
               (name chart)
               (apply #'uiop:strcat 
                      (mapcar #'(lambda (row) (format nil "{~{ ~f~} }~&" row)) xypairs))
               (string-downcase (string colour))))

;; ---------------------------------------------------------------------------
;; Box Plot

(defclass box-plot (plotchart)
  ((xdata :accessor xdata :initarg :xdata :initform nil)
   (ydata :accessor ydata :initarg :ydata :initform nil)
   (orientation :accessor orientation :initarg :orientation :initform :horizontal)))

(defmethod initialize-instance :after ((chart box-plot) &key)
  (format-wish "set ~a [::Plotchart::createBoxplot ~a {~{ ~a~} } {~{ ~a~} } ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (xdata chart)
               (ydata chart)
               (string-downcase (string (orientation chart)))
               ))

(defgeneric box-plot-plot (chart series label values))
(defmethod box-plot-plot ((chart box-plot) series label values)
  (format-wish "$~a plot ~a ~a { ~{ ~a~} }" 
               (name chart) series label values))

;; ---------------------------------------------------------------------------
;; Time Chart

(defclass time-chart (plotchart)
  ((time-begin :accessor time-begin :initarg :time-begin :initform nil)
   (time-end :accessor time-end :initarg :time-end :initform nil)))

(defmethod initialize-instance :after ((chart time-chart) &key num-items barheight ylabelwidth)
  (format-wish "set ~a [::Plotchart::createTimechart ~a \"~a\" \"~a\" ~a~a~a]"
               (name chart)
               (widget-path (canvas chart))
               (time-begin chart)
               (time-end chart)
               (if num-items num-items "")
               (if barheight (format nil " -barheight ~d" barheight) "")
               (if ylabelwidth (format nil " -ylabelwidth ~d" ylabelwidth) "")))

(defgeneric time-chart-period (chart text time-begin time-end &optional colour))
(defmethod time-chart-period ((chart time-chart) text time-begin time-end &optional colour)
  (format-wish "$~a period \"~a\" \"~a\" \"~a\" ~a" 
               (name chart) text time-begin time-end
               (if colour (string-downcase (string colour)) "")))

(defgeneric time-chart-addperiod (chart time-begin time-end &optional colour))
(defmethod time-chart-addperiod ((chart time-chart) time-begin time-end &optional colour)
  (format-wish "$~a addperiod \"~a\" \"~a\" ~a" 
               (name chart) time-begin time-end
               (if colour (string-downcase (string colour)) "")))

(defgeneric time-chart-milestone (chart text time &optional colour))
(defmethod time-chart-milestone ((chart time-chart) text time &optional colour)
  (format-wish "$~a milestone \"~a\" \"~a\" ~a" 
               (name chart) text time
               (if colour (string-downcase (string colour)) "")))

(defgeneric time-chart-addmilestone (chart time &optional colour))
(defmethod time-chart-addmilestone ((chart time-chart) time &optional colour)
  (format-wish "$~a addmilestone \"~a\" ~a" 
               (name chart) time
               (if colour (string-downcase (string colour)) "")))

(defgeneric time-chart-vertline (chart text time &optional colour))
(defmethod time-chart-vertline ((chart time-chart) text time &optional colour)
  (format-wish "$~a vertline \"~a\" \"~a\" ~a" 
               (name chart) text time
               (if colour (string-downcase (string colour)) "")))

(defgeneric time-chart-hscroll (chart hscroll))
(defmethod time-chart-hscroll ((chart time-chart) hscroll)
  (format-wish "$~a hscroll ~a" (name chart) (name hscroll)))

(defgeneric time-chart-vscroll (chart vscroll))
(defmethod time-chart-vscroll ((chart time-chart) vscroll)
  (format-wish "$~a vscroll ~a" (name chart) (name vscroll)))

;; ---------------------------------------------------------------------------
;; Gantt Chart

(defclass gantt-chart (plotchart)
  ((time-begin :accessor time-begin :initarg :time-begin :initform nil)
   (time-end :accessor time-end :initarg :time-end :initform nil)))

(defmethod initialize-instance :after ((chart gantt-chart) &key num-items max-width barheight ylabelwidth)
  (format-wish "set ~a [::Plotchart::createTimechart ~a \"~a\" \"~a\" ~a ~a ~a ~a]"
               (name chart)
               (widget-path (canvas chart))
               (time-begin chart)
               (time-end chart)
               (if num-items num-items (if max-width "1" "")) ; note, max-width must be second
               (if max-width max-width "")
               (if barheight (format nil "-barheight ~d" barheight) "")
               (if ylabelwidth (format nil "-ylabelwidth ~d" ylabelwidth) "")))

(defgeneric gantt-chart-task (chart text time-begin time-end completed))
(defmethod gantt-chart-task ((chart gantt-chart) text time-begin time-end completed)
  (format-wish "$~a task \"~a\" \"~a\" \"~a\" ~f" 
               (name chart) text time-begin time-end completed))

(defgeneric gantt-chart-milestone (chart text time &optional colour))
(defmethod gantt-chart-milestone ((chart gantt-chart) text time &optional colour)
  (format-wish "$~a milestone \"~a\" \"~a\" ~a" 
               (name chart) text time
               (if colour (string-downcase (string colour)) "")))

(defgeneric gantt-chart-vertline (chart text time))
(defmethod gantt-chart-vertline ((chart gantt-chart) text time)
  (format-wish "$~a vertline \"~a\" \"~a\"" 
               (name chart) text time))

(defgeneric gantt-chart-connect (chart from to))
(defmethod gantt-chart-connect ((chart gantt-chart) from to)
  (format-wish "$~a connect {~{ ~a~} } {~{ ~a~} }" 
               (name chart) from to))

(defgeneric gantt-chart-summary (chart text args))
(defmethod gantt-chart-summary ((chart gantt-chart) text args)
  (format-wish "$~a summary \"~a\" {~{ ~a~} }" 
               (name chart) text args))

(defgeneric gantt-chart-color (chart keyword newcolour))
(defmethod gantt-chart-color ((chart gantt-chart) keyword newcolour)
  (format-wish "$~a color ~a ~a" 
               (name chart) 
               (string-downcase (string keyword))
               (string-downcase (string newcolour))))

(defgeneric gantt-chart-colour (chart keyword newcolour))
(defmethod gantt-chart-colour ((chart gantt-chart) keyword newcolour)
  (format-wish "$~a color ~a ~a" 
               (name chart) 
               (string-downcase (string keyword))
               (string-downcase (string newcolour))))

(defgeneric gantt-chart-font (chart keyword newfont))
(defmethod gantt-chart-font ((chart gantt-chart) keyword newfont)
  (format-wish "$~a font ~a \"~a\"" 
               (name chart) 
               (string-downcase (string keyword))
               newfont))

(defgeneric gantt-chart-hscroll (chart hscroll))
(defmethod gantt-chart-hscroll ((chart gantt-chart) hscroll)
  (format-wish "$~a hscroll ~a" (name chart) (name hscroll)))

(defgeneric gantt-chart-vscroll (chart vscroll))
(defmethod gantt-chart-vscroll ((chart gantt-chart) vscroll)
  (format-wish "$~a vscroll ~a" (name chart) (name vscroll)))

;; ---------------------------------------------------------------------------
;; Right Axis

(defclass right-axis (plotchart)
  ((yaxis :accessor yaxis :initarg :yaxis :initform '(0 10 1))))

(defmethod initialize-instance :after ((chart right-axis) &key)
  (format-wish "set ~a [::Plotchart::createRightAxis ~a {~{ ~d~} }]" 
               (name chart) 
               (widget-path (canvas chart))
               (yaxis chart)))

(defgeneric right-axis-plot (chart series x-coord y-coord))
(defmethod right-axis-plot ((chart right-axis) series x-coord y-coord)
  "Adds a data point to the chart"
  (format-wish "$~a plot ~a ~f ~f" (name chart) series x-coord y-coord))

(defgeneric right-axis-dataconfig (chart series &key colour color type symbol
                                      radius width filled fillcolour style))
(defmethod right-axis-dataconfig ((chart right-axis) series 
                                                     &key colour color type
                                                     symbol radius width filled
                                                     fillcolour style)
  (format-wish "$~a dataconfig ~a ~a" 
               (name chart) 
               series
               (make-config-args colour color type symbol radius width filled
                                 fillcolour style)))

;; ---------------------------------------------------------------------------
;; Table Chart

(defclass table-chart (plotchart)
  ((columns :accessor columns :initarg :columns :initform nil)
   (widths :accessor widths :initarg :widths :initform nil)))

(defmethod initialize-instance :after ((chart table-chart) &key)
  (format-wish "set ~a [::Plotchart::createTableChart ~a {~{ ~d~} } ~a]" 
               (name chart) 
               (widget-path (canvas chart))
               (columns chart)
               (if (widths chart)
                 (if (listp (widths chart))
                   (format nil "{~{ ~a~} }" (widths chart)) ; either a list 
                   (widths chart)) ; or constant width for all columns
                 "")))

(defgeneric table-chart-row (chart items))
(defmethod table-chart-row ((chart table-chart) items)
  (format-wish "$~a row {~{ \"~a\"~} }" (name chart) items))

(defgeneric table-chart-separator (chart))
(defmethod table-chart-separator ((chart table-chart))
  (format-wish "$~a row" (name chart)))

(defgeneric table-chart-cellconfigure (chart &key background cell font anchor justify))
(defmethod table-chart-cellconfigure ((chart table-chart) &key background cell font anchor justify)
  (format-wish "$~a cellconfigure ~a"
               (name chart)
               (let ((result ""))
                 (when background
                   (setf result (format nil "~a -background ~a" result (string-downcase (string background)))))
                 (when cell
                   (setf result (format nil "~a -cell ~a" result (string-downcase (string cell)))))
                 (when font
                   (setf result (format nil "~a -font \"~a\"" result (string-downcase (string font)))))
                 (when anchor
                   (setf result (format nil "~a -anchor ~a" result (string-downcase (string result)))))
                 (when justify
                   (setf result (format nil "~a -justify ~a" result (string-downcase (string justify)))))
                 result)))

;; ---------------------------------------------------------------------------
;; Ternary Diagram

(defclass ternary-diagram (plotchart)
  ())

(defmethod initialize-instance :after ((chart ternary-diagram) &key box axesbox fractions steps)
  (format-wish "set ~a [::Plotchart::createTernaryDiagram ~a ~a"
               (name chart)
               (widget-path (canvas chart))
               (let ((result ""))
                 (when box 
                   (setf result (format nil "~a -box {~{ ~a~} }" result box)))
                 (when axesbox
                   (setf result (format nil "~a -axesbox {~{ ~a~} }" result axesbox)))
                 (when fractions
                   (setf result (format nil "~a -fractions ~a" 
                                        result 
                                        (string-downcase (string fractions)))))
                 (when steps
                   (setf result (format nil "~a -step ~d" result steps)))
                 result)))

(defgeneric ternary-diagram-plot (chart series xcrd ycrd zcrd text &optional direction))
(defmethod ternary-diagram-plot ((chart ternary-diagram) series xcrd ycrd zcrd text &optional direction)
  (format-wish "$~a plot ~a ~f ~f ~f \"~a\" ~a"
               (name chart) series xcrd ycrd zcrd text
               (if direction direction "")))

(defgeneric ternary-diagram-line (chart series coords))
(defmethod ternary-diagram-line ((chart ternary-diagram) series coords)
  "Draw a continuous line given a series of coordinates (triplets)"
  (format-wish "$~a line ~a {~a}"
               (name chart) 
               series 
               (let ((result ""))
                 (dolist (coord coords)
                   (setf result (format nil "~a {~{ ~a~} }" result coord)))
                 result)))

(defgeneric ternary-diagram-fill (chart series coords))
(defmethod ternary-diagram-fill ((chart ternary-diagram) series coords)
  "Fill a polygon defined by series of coordinates (triplets)"
  (format-wish "$~a fill ~a {~a}"
               (name chart) 
               series 
               (let ((result ""))
                 (dolist (coord coords)
                   (setf result (format nil "~a {~{ ~a~} }" result coord)))
                 result)))

(defgeneric ternary-diagram-text (chart xtext ytext ztext))
(defmethod ternary-diagram-text ((chart ternary-diagram) xtext ytext ztext)
  "Displays given text at each corner"
  (format-wish "$~a text \"~a\" \"~a\" \"~a\""
               (name chart) xtext ytext ztext))

(defgeneric ternary-diagram-ticklines (chart &optional colour))
(defmethod ternary-diagram-ticklines ((chart ternary-diagram) &optional colour)
  "Displays ticklines in optional colour"
  (format-wish "$~a ticklines ~a"
               (name chart)
               (if colour (string-downcase (string colour)) "")))

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
  ((xaxis :accessor xaxis :initarg :xaxis :initform '(0 10 1))
   (ylabels :accessor ylabels :initarg :ylabels :initform nil)))

(defmethod initialize-instance :after ((chart status-timeline) &key box axesbox showxaxis)
  (format-wish "set ~a [::Plotchart::createStatusTimeline ~a {~{ ~a~} } {~{ ~a~} }~a"
               (name chart)
               (widget-path (canvas chart))
               (xaxis chart)
               (ylabels chart)
               (let ((result ""))
                 (when box 
                   (setf result (format nil "~a -box {~{ ~a~} }" result box)))
                 (when axesbox
                   (setf result (format nil "~a -axesbox {~{ ~a~} }" result axesbox)))
                 (when showxaxis
                   (setf result (format nil "~a -xaxis ~a" 
                                        result 
                                        (string-downcase (string showxaxis)))))
                 result)))

(defgeneric status-timeline-plot (chart series item start stop &optional colour))
(defmethod status-timeline-plot ((chart status-timeline) series item start stop &optional colour)
  "Draws a bar in given colour from start to stop"
  (format-wish "$~a plot ~a \"~a\" ~f ~f ~a"
               (name chart)
               series 
               item
               start 
               stop
               (if colour (string-downcase (string colour)) "")))

(defgeneric status-timeline-vertline (chart text time))
(defmethod status-timeline-vertline ((chart status-timeline) text time)
  "Draws a vertical line to mark a significant moment"
  (format-wish "$~a vertline ~a \"~a\" ~f"
               (name chart) text time))

