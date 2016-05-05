package Core

import Constraints.Constraint
import edu.umass.cs.smtlib.SMT
import smtlib.parser.Terms

case class Solver(constraints: Seq[Constraint], debug: Boolean = false) {
  private val solver = SMT()
  init()

  private def init(): Unit = {
    val defs = constraints.flatMap(_.definition)
    val asserts = constraints.flatMap(_.asserts)

    if (debug) {
      println("; DEFINITIONS:")
      defs.foreach(println)
      println("; ASSERTIONS:")
      asserts.foreach(println)
    }

    defs.foreach(solver.eval)
    asserts.foreach(solver.eval)
  }

  def checkSAT : Boolean = solver.checkSat()
  def model : Option[List[Terms.SExpr]] =
    if (solver.checkSat()) {
      Some(solver.getModel())
    } else {
      None
    }
}
