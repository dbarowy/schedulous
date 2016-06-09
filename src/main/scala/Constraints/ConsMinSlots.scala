package Constraints

import smtlib.parser.Commands.{Assert, FunDef, DefineFun, Command}
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._

// Constraint #3: At least minslots per person
case class ConsMinSlots(minslots: Int, peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
  private val (fname,fdef,assertions) = init()

  private def init() : (SSymbol,DefineFun,List[Assert]) = {
    val fname = SSymbol(this.getClass.getName)
    val arg_person = SortedVar(SSymbol("person"), IntSort())
    val arg_numslots = SortedVar(SSymbol("numslots"), IntSort())
    val literals: Seq[Term] = slotmap.map { case (symb,_) =>
      ITE(
        Equals(
          QualifiedIdentifier(SimpleIdentifier(symb)),
          QualifiedIdentifier(SimpleIdentifier(arg_person.name))
        ),
        NumeralLit(1),
        NumeralLit(0)
      )
    }.toSeq
    val exprReducer = (lhs: Term, rhs: Term) => smtlib.theories.Ints.Add(lhs,rhs)
    val expr = GreaterEquals(
      literals.reduce(exprReducer),
      QualifiedIdentifier(SimpleIdentifier(arg_numslots.name))
    )

    val assertions = peoplemap.map { case (person,_) =>
      Assert(
        FunctionApplication(QualifiedIdentifier(SimpleIdentifier(fname)),
          Seq(
            NumeralLit(person),
            NumeralLit(minslots)
          )
        )
      )
    }.toList

    (
      fname,
      DefineFun(FunDef(fname, Seq(arg_person, arg_numslots), BoolSort(), expr)),
      assertions
    )
  }

  override def asserts: List[Assert] = {
    assert(assertions.nonEmpty, "ERROR: There should always be at least one person.")
    assertions
  }
  override def definition: List[Command] = List(fdef)
}
