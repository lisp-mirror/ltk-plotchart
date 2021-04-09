(require 'asdf)
(require 'ltk-plotchart)
(use-package :ltk)

(with-ltk ()
          (wm-title *tk* "threed-bar-chart-example.lisp")
          (let* ((canvas (make-instance 'canvas :width 400 :height 400 
                                        :background :white))
                 (bar-3d (chart:create-3d-bar-chart canvas '(0 60 5) 
                                                    :num-bars 7)))

            (grid canvas 0 0)

            (chart:title-text bar-3d "Number of Moons per Planet")

            (chart:config bar-3d :label-font "Times 8" :use-background t)

            (chart:plot bar-3d "Earth" 1 :blue)
            (chart:plot bar-3d "Mars" 2 :red)
            (chart:plot bar-3d "Jupiter" 53 :orange)
            (chart:plot bar-3d "Saturn" 53 :yellow)
            (chart:plot bar-3d "Uranus" 27 :green)
            (chart:plot bar-3d "Neptune" 13 :cyan)
            (chart:plot bar-3d "Pluto" 5 :grey)
            ))
