(in-package :ltk-plotchart)

(defun check (query error-message)
  (if query
    (format t ".")
    (format t "~&Error: ~a~&" error-message)))

(defmacro check-no-error (expr msg)
  `(check (handler-case ,expr
            (error (c) (declare (ignore c))
                   nil))
          ,msg))

(defmacro check-error (expr msg)
  `(check (handler-case (prog2 ,expr nil)
            (error (c) (declare (ignore c))
                   t))
          ,msg))

(defun test-tcl-it ()
  (check (string= "red" (tcl-it "red"))
         "tcl-it: failed with red")
  ; put quotes around multi-word strings
  (check (string= "\"Dark Red\"" (tcl-it "Dark Red")) 
         "tcl-it: failed with Dark Red")
  ; convert keywords/symbols to strings and downcase
  (check (string= "red" (tcl-it :red))
         "tcl-it: failed with :red")
  (check (string= "red" (tcl-it 'RED))
         "tcl-it: failed with 'RED")
  ; check floats/ints
  (check (string= "0.5" (tcl-it 0.5))
         "tcl-it: failed with 0.5")
  (check (string= "5" (tcl-it 5))
         "tcl-it: failed with 5")
  ; check lists
  (check (string= "{ red blue }" (tcl-it '(:red "blue")))
         "tcl-it: failed with '(:red \"blue\")")
  ; check lists of lists
  (check (string= "{ { 1 2 } { 3 4 } }" (tcl-it '((1 2) (3 4))))
         "tcl-it: failed with '((1 2) (3 4))")
  (check (string= "" (tcl-it nil))
         "tcl-it: failed with nil")
  ; fake booleans
  (check (string= "1" (tcl-it-boolean t))
         "tcl-it-boolean: failed with 't")
  (check (string= "0" (tcl-it-boolean nil))
         "tcl-it-boolean: failed with 'nil")
  )

(defun test-tcl-it-checked ()
  (check-no-error (string= "red" 
                           (tcl-it-checked :red '("red" "blue" "green") "colours"))
                  "tcl-it-checked: failed on valid :red")
  (check-no-error (string= "red" 
                           (tcl-it-checked "RED" '("red" "blue" "green") "colours"))
                  "tcl-it-checked: failed on valid 'RED'")
  (check-error (string= "red" 
                        (tcl-it-checked :red '("blue" "green") "colours"))
               "tcl-it-checked: failed on missing :red")
  )

(defun test-make-xy-args ()
  (check (string= "" (make-xy-args nil nil nil nil nil nil))
         "make-xy-args: failed with nil's")
  (check (string= "-xlabels { red blue green } "
                  (make-xy-args '(red blue green) nil nil nil nil nil nil))
         "make-xy-args: xlabels")
  (check (string= "-ylabels { red blue green } "
                  (make-xy-args nil '(red blue green) nil nil nil nil nil nil))
         "make-xy-args: ylabels")
  (check (string= "-xlabels { a b c } -ylabels { red blue green } "
                  (make-xy-args '("a" "b" :c) '(red blue green) nil nil nil nil nil nil))
         "make-xy-args: xlabels+ylabels")
  (check (string= "-xlabels { red blue green } -axesatzero 1 "
                  (make-xy-args '(red blue green) nil nil nil nil nil t nil))
         "make-xy-args: xlabels + axesatzero")
  (check (string= "-box { 0 0 100 50 } -axesbox { wc n 0 0 100 50 } "
                  (make-xy-args nil nil '(0 0 100 50) '("wc" :n 0 0 100 50) nil nil))
         "make-xy-args: box + axesbox")
  (check (string= "-timeformat %H:%M -gmt 1 "
                  (make-xy-args nil nil nil nil "%H:%M" t nil nil))
         "make-xy-args: timeformat + gmt")
  )

(defun test-validators ()
  (check-no-error (valid-axis-p '(1 10 2) "a")
         "validators: fail on '(1 10 2)")
  (check-error (valid-axis-p '(10 2) "a")
         "validators: fail on '(a 10 2)")
  (check-error (valid-axis-p '(1 10 2 3) "a")
         "validators: fail on '(a 10 2)")
  (check-error (valid-axis-p '(a 10 2) "a")
         "validators: fail on '(a 10 2)")

  (check-error (valid-number-pair-p '(1 10 2) "a")
         "validators: fail on '(1 10 2)")
  (check-no-error (valid-number-pair-p '(10 2) "a")
         "validators: fail on '(a 10 2)")

  (check-error (valid-number-pair-p '(1 10 2) "a")
               "validators: fail on '(1 10 2) radius data")
  (check-no-error (valid-number-pair-p '(1 10) "a")
                  "validators: fail on '(1 10) radius data")

  (check-no-error (valid-time-axis-p '("start" "end" 10) "a")
                  "validators: fail on '(start end 10) time")
  (check-error (valid-time-axis-p '(10 "start" "end") "a")
                  "validators: fail on '(10 start end) time")
  (check-error (valid-time-axis-p '("start" 10 "end") "a")
                  "validators: fail on '(start 10 end) time")
  (check-error (valid-time-axis-p '("start" "end") "a")
                  "validators: fail on '(start end) time")
  )

(defun run-tests ()
  (format t "~&Testing: ") 
  (test-tcl-it)
  (test-tcl-it-checked)
  (test-make-xy-args)
  (test-validators)
  (format t "~%-------- Done~&"))
