package Constraints

import smtlib.parser.Commands.{Command, Assert, FunDef, DefineFun}
import smtlib.parser.Terms._
import smtlib.theories.Core
import smtlib.theories.Core._
import smtlib.theories.Ints.NumeralLit

// Constraint #1: This slot must be filled
case class ConsFillSlots(peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
  private val (fname,fdef,assertions) = init()

  private def init() : (SSymbol,DefineFun,List[Assert]) = {
    val fname = SSymbol(this.getClass.getName)
    // nop
    val fdef = DefineFun(FunDef(fname, Seq(), BoolSort(), Equals(NumeralLit(1), NumeralLit(1))))
    val assertions =
    slotmap.map { case (slot,ds) =>
      val literals = peoplemap.flatMap { case (i,person) =>
        if (person.availableFor(ds)) {
          Some(
            Equals(
              QualifiedIdentifier(SimpleIdentifier(slot)),
              NumeralLit(i)
            )
          )
        } else {
          None
        }
      }.toSeq
      val expr = Core.Or(literals)
      Assert(expr)
    }.toList

    (fname,fdef,assertions)
  }

  def definition : List[Command] = List(fdef)
  def asserts: List[Assert] = assertions
}
//
//package Constraints
//
//import smtlib.parser.Commands.{Command, Assert, FunDef, DefineFun}
//import smtlib.parser.Terms._
//import smtlib.theories.Core
//import smtlib.theories.Core._
//import smtlib.theories.Ints.{NumeralLit, IntSort}
//
//// Constraint #1: This slot must be filled
//case class ConsFillSlots(peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
//  private val (fname,fdef,assertions) = init()
//
//  private def init() : (SSymbol,DefineFun,List[Assert]) = {
//    val fname = SSymbol(this.getClass.getName)
//    val slotarg = SortedVar(SSymbol("slot"), IntSort())
//    val literals = peoplemap.map { case (i,_) =>
//      Equals(
//        QualifiedIdentifier(SimpleIdentifier(slotarg.name)),
//        NumeralLit(i)
//      )
//    }.toSeq
//    val expr = Core.Or(literals)
//    val fdef = DefineFun(FunDef(fname, Seq(slotarg), BoolSort(), expr))
//    val assertions =
//      slotmap.map { case (slot,_) =>
//        Assert(
//          FunctionApplication(QualifiedIdentifier(SimpleIdentifier(fname)),
//            Seq(QualifiedIdentifier(SimpleIdentifier(slot))))
//        )
//      }.toList
//
//    (fname,fdef,assertions)
//  }
//
//  def definition : List[Command] = List(fdef)
//  def asserts: List[Assert] = assertions
//}
