(defsystem "ltk-plotchart"
  :components ((:file "src/package")
               (:file "src/ltk-plotchart" :depends-on ("src/package")))
  :depends-on ("ltk"))
