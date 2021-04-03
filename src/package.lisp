(defpackage :ltk-plotchart
  (:use :cl :ltk)
  (:nicknames :chart)
  (:export
    ; classes 
    :bar-chart
    :box-plot
    :gantt-chart
    :histogram
    :horizontal-bar-chart
    :isometric-plot
    :logx-logy-plot
    :logx-y-plot
    :pie-chart
    :polar-plot
    :radial-chart
    :right-axis
    :spiral-pie
    :status-timeline
    :strip-chart
    :table-chart
    :ternary-diagram
    :threed-bar-chart
    :threed-plot
    :threed-ribbon-chart
    :threed-ribbon-plot
    :time-chart
    :tx-plot
    :windrose
    :xy-plot
    :x-logy-plot
    ; create-X functions
    :create-bar-chart
    :create-box-plot
    :create-gantt-chart
    :create-histogram
    :create-horizontal-bar-chart
    :create-isometric-plot
    :create-logx-logy-plot
    :create-logx-y-plot
    :create-pie-chart
    :create-polar-plot
    :create-radial-chart
    :create-right-axis
    :create-spiral-pie
    :create-status-timeline
    :create-strip-chart
    :create-3d-bar-chart
    :create-3d-plot
    :create-3d-ribbon-chart
    :create-3d-ribbon-plot
    :create-table-chart
    :create-ternary-diagram
    :create-time-chart
    :create-tx-plot
    :create-windrose
    :create-xy-plot
    :create-x-logy-plot
    ; remaining functions
    :add-milestone
    :add-period
    :area
    :background
    :balloon
    :balloon-config
    :box-and-whiskers
    :cell-configure
    :color 
    :colors
    :color-map
    :colour
    :colours
    :colour-map
    :config
    :connect
    :corner-text
    :data-config
    :dot-config
    :draw-circle
    :draw-contour-lines
    :draw-contour-lines-function-values
    :draw-dot
    :draw-filled-circle
    :draw-filled-polygon
    :draw-filled-rectangle
    :draw-grid
    :draw-label-dot
    :draw-line
    :draw-rectangle
    :draw-vector
    :draw-x-band
    :draw-y-band
    :explode
    :font
    :gridsize
    :horizontal-scrollbar
    :interpolate-data
    :interval
    :legend
    :legend-config
    :legend-isometric-lines
    :legend-shades
    :milestone
    :minmax
    :period
    :plaintext
    :plaintext-config
    :plot
    :plot-cumulative
    :plot-data
    :plot-list
    :plot-pack
    :rchart
    :region
    :remove-from-legend
    :ribbon
    :row
    :save-plot
    :separator
    :subtitle
    :summary
    :task
    :ticklines
    :title-text
    :trend
    :v-subtext
    :v-text
    :vector-config
    :vertical-scrollbar
    :vertical-line
    :x-config
    :x-subtext
    :x-text
    :x-ticklines
    :y-config
    :y-subtext
    :y-text
    :y-ticklines
    ))
