// Note: "Run this worksheet" does not recompile other files in this project,
// You should run "~compile" in sbt to automatically recompile the project.

import interpreter._
import Lisp._

object Code {
  val code1 =
"""
(* (+ 3 3) 7)
"""

  val code2 =
"""
(def factorial (lambda (x)
  (if  (= x 0) 1 (* x (factorial (- x 1)))))
(factorial 6))
"""
}
import Code._

// Uncomment to enable tracing of each evaluation step
// trace = true

// Display the parser output
println(string2lisp(code1))
// Run the interpreter
println(evaluate(code1))
