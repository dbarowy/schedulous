package Constraints

import java.time.Duration

import smtlib.parser.Commands.{FunDef, DefineFun, Assert, Command}
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.theories.Reals.{DecimalLit, RealSort}

// Constraint #6: Ensure that all worker average workloads are within epsilon of the global average
case class ConsAvgWorkload(minute_epsilon: Int, workloadfn: SSymbol, peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
  val (fname,fdef,assertions) = init()

  private def init() : (SSymbol,DefineFun,List[Assert]) = {
    val num_people = peoplemap.size
    assert(num_people > 0, "You cannot schedule zero people.")

    val fname = SSymbol(this.getClass.getName)
    val arg_epsilon = SortedVar(SSymbol("epsilon"), RealSort())
    val literals = peoplemap.map { case (person,_) =>
      FunctionApplication(
        QualifiedIdentifier(SimpleIdentifier(workloadfn)),
        Seq(NumeralLit(person))
      )
    }.toSeq

    val exprReducer = (lhs: Term, rhs: Term) => smtlib.theories.Ints.Add(lhs,rhs)
    val total_workload = literals.reduce(exprReducer)
    val avg_workload = Div(total_workload, NumeralLit(num_people))
    val fair_workload = slotmap.map {
      case (_,slot) => Duration.between(slot.start, slot.end).toMinutes.toDouble
    }.sum / peoplemap.size

    val expr = And(
      GreaterEquals(
        avg_workload,
        Sub(
          DecimalLit(fair_workload),
          QualifiedIdentifier(SimpleIdentifier(arg_epsilon.name))
        )
      ),
      LessEquals(
        avg_workload,
        Add(
          DecimalLit(fair_workload),
          QualifiedIdentifier(SimpleIdentifier(arg_epsilon.name))
        )
      )
    )

    val fdef = DefineFun(FunDef(fname, Seq(arg_epsilon), BoolSort(), expr))

    val assertions =
      slotmap.map { case (slot,_) =>
        Assert(
          FunctionApplication(
            QualifiedIdentifier(SimpleIdentifier(fname)),
            Seq(DecimalLit(minute_epsilon)))
        )
      }.toList

    (fname, fdef, assertions)
  }

  def asserts: List[Assert] = assertions
  def definition: List[Command] = List(fdef)
}
