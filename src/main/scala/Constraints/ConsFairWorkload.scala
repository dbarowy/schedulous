package Constraints

import java.time.Duration

import smtlib.parser.Commands.{FunDef, DefineFun, Assert, Command}
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.theories.Reals.{DecimalLit, RealSort}

// Constraint #6: Ensure that all worker average workloads are within epsilon of the fairest allocation
case class ConsFairWorkload(minute_epsilon: Int, workloadfn: SSymbol, peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
  val (fname,fdef,assertions) = init()

  private def wlExpr(person: Int, fair_workload: Double, arg_epsilon: SortedVar) : Term = {
    And(
      GreaterEquals(
        FunctionApplication(
          QualifiedIdentifier(SimpleIdentifier(workloadfn)),
          Seq(NumeralLit(person))
        ),
        Sub(
          DecimalLit(fair_workload),
          QualifiedIdentifier(SimpleIdentifier(arg_epsilon.name))
        )
      ),
      LessEquals(
        FunctionApplication(
          QualifiedIdentifier(SimpleIdentifier(workloadfn)),
          Seq(NumeralLit(person))
        ),
        Add(
          DecimalLit(fair_workload),
          QualifiedIdentifier(SimpleIdentifier(arg_epsilon.name))
        )
      )
    )
  }

  private def init() : (SSymbol,DefineFun,List[Assert]) = {
    val num_people = peoplemap.size
    assert(num_people > 0, "You cannot schedule zero people.")

    val fname = SSymbol(this.getClass.getName)
    val arg_epsilon: SortedVar = SortedVar(SSymbol("epsilon"), RealSort())

    val fair_workload: Double = slotmap.map {
      case (_,slot) => Duration.between(slot.start, slot.end).toMinutes.toDouble
    }.sum / peoplemap.size

    val expr : Term =
      if (peoplemap.size > 1) {
        val first = wlExpr(peoplemap.keys.head, fair_workload, arg_epsilon)
        peoplemap.keys.tail.foldLeft(first) { case (acc: Term, p: Int) =>
          And(acc, wlExpr(p, fair_workload, arg_epsilon))
        }
      } else {
        wlExpr(peoplemap.keys.head, fair_workload, arg_epsilon)
      }

    val fdef = DefineFun(FunDef(fname, Seq(arg_epsilon), BoolSort(), expr))

    val assertions =
      List(
        Assert(
          FunctionApplication(
            QualifiedIdentifier(SimpleIdentifier(fname)),
            Seq(DecimalLit(minute_epsilon)))
        )
      )

    (fname, fdef, assertions)
  }

  def asserts: List[Assert] = assertions
  def definition: List[Command] = List(fdef)
}
