package Constraints

import java.time.Duration

import Core.{Approved, Schedule}
import smtlib.parser.Commands.{Assert, Command, DefineFun, FunDef}
import smtlib.parser.Terms._
import smtlib.theories.Core.{Equals, ITE}
import smtlib.theories.Ints.{Add, IntSort}
import smtlib.theories.Reals.{DecimalLit, RealSort}

// Constraint #5: Helper function to compute a person's workload in minutes.
case class ConsWorkload(peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
  val (fname,fdef,assertions) = init()

  private def init() : (SSymbol,DefineFun,List[Assert]) = {
    val fname = SSymbol(this.getClass.getName)
    val arg_person = SortedVar(SSymbol("person"), IntSort())

    val exprReducer = (lhs: Term, rhs: Term) => smtlib.theories.Ints.Add(lhs,rhs)

    val literals = slotmap.map { case (symb,slot) =>
      ITE(
        Equals(
          QualifiedIdentifier(SimpleIdentifier(symb)),
          QualifiedIdentifier(SimpleIdentifier(arg_person.name))
        ),
        DecimalLit(Duration.between(slot.start,slot.end).toMinutes.toDouble),
        DecimalLit(0)
      )
    }.toSeq

    val expr = literals.reduce(exprReducer)
    val fdef = DefineFun(FunDef(fname, Seq(arg_person), RealSort(), expr))

    (fname, fdef, List.empty)
  }

  def asserts: List[Assert] = {
    assert(assertions.nonEmpty, "ERROR: There should always be at least one workload.")
    assertions
  }
  def definition: List[Command] = List(fdef)
  def name: SSymbol = fname
}
