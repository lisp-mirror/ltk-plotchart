(defsystem "ltk-plotchart"
  :description "A wrapper around tklib's plotchart library to work with LTk."
  :version "0.1"
  :author "Peter Lane <peterlane@gmx.com>"
  :maintainer "Peter Lane <peterlane@gmx.com>"
  :homepage "https://peterlane.netlify.app/ltk-plotchart/"
  :licence "MIT"
  :components ((:file "src/package")
               (:file "src/ltk-plotchart" :depends-on ("src/package")))
  :depends-on ("ltk")
  :in-order-to ((test-op (test-op "ltk-plotchart/tests"))))

(defsystem "ltk-plotchart/tests"
  :components ((:file "src/tests"))
  :depends-on ("ltk-plotchart")
  :perform (test-op (o c) (symbol-call :ltk-plotchart :run-tests)))
 
