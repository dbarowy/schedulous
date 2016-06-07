package Constraints

import Core.Day
import smtlib.parser.Commands.{Assert, FunDef, DefineFun, Command}
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._

// Constraint #4: No more than d days per person
case class ConsMaxDays(maxdays: Int, days: Set[Day], peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
  private val (fname,fdef,assertions) = init()

  private def init() : (SSymbol,DefineFun,List[Assert]) = {
    val fname = SSymbol(this.getClass.getName)
    val arg_person = SortedVar(SSymbol("person"), IntSort())
    val arg_days = SortedVar(SSymbol("days"), IntSort())

    val slotmap_inv = Util.invertMap(slotmap)

    val terms = days.map { day =>
      val subexprs = day.dateslots.map { slot =>
        val symb = slotmap_inv(slot)
        Equals(
          QualifiedIdentifier(SimpleIdentifier(symb)),
          QualifiedIdentifier(SimpleIdentifier(arg_person.name))
        )
      }.toSeq

      if (subexprs.size > 1) {
        ITE(Or(subexprs), NumeralLit(1), NumeralLit(0))
      } else {
        ITE(subexprs.head, NumeralLit(1), NumeralLit(0))
      }
    }

    val exprReducer = (lhs: Term, rhs: Term) => smtlib.theories.Ints.Add(lhs,rhs)
    val expr = LessEquals(
      terms.reduce(exprReducer),
      QualifiedIdentifier(SimpleIdentifier(arg_days.name))
    )

    val assertions = peoplemap.map { case (person,_) =>
      Assert(
        FunctionApplication(QualifiedIdentifier(SimpleIdentifier(fname)),
          Seq(
            NumeralLit(person),
            NumeralLit(maxdays)
          )
        )
      )
    }.toList

    (
      fname,
      DefineFun(FunDef(fname, Seq(arg_person, arg_days), BoolSort(), expr)),
      assertions
    )
  }

  def asserts: List[Assert] = assertions
  def definition: List[Command] = List(fdef)
}
