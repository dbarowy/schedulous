package Constraints

import Core._
import smtlib.parser.Commands.{Assert, Command, DefineFun, FunDef}
import smtlib.parser.Terms._
import smtlib.theories.Core.{BoolSort, Equals, Or}
import smtlib.theories.Ints.NumeralLit

// Constraint #1: This slot must be filled
case class ConsFillSlots(peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap, oldSchedule: Option[Schedule]) extends Constraint {
  private val assertions = init()

  private def literalsNoAssignment(slotSymbol: SSymbol, ds: Dateslot) : Seq[Term] = {
    peoplemap.flatMap { case (i,person) =>
      if (person.availableFor(ds)) {
        Some(
          Equals(
            QualifiedIdentifier(SimpleIdentifier(slotSymbol)),
            NumeralLit(i)
          )
        )
      } else {
        None
      }
    }.toSeq
  }

  private def literalsWithAssignment(slotSymbol: SSymbol, ds: Dateslot, assignment: Assignment) : Seq[Term] = {
    assignment.approval match {
      case Unapproved => literalsNoAssignment(slotSymbol, ds)
      case Approved => Seq()
      case Rejected =>
        peoplemap.flatMap { case (i,person) =>
          if (person.availableFor(ds) && person != assignment.person) {
            Some(
              Equals(
                QualifiedIdentifier(SimpleIdentifier(slotSymbol)),
                NumeralLit(i)
              )
            )
          } else {
            None
          }
        }.toSeq
    }
  }

  private def init() : List[Assert] = {
    val fname = SSymbol(this.getClass.getName)
    val assertions =
    slotmap.map { case (slot: SSymbol, ds: Dateslot) =>
      val literals: Seq[Term] = oldSchedule match {
        case Some(schedule) =>
          // old schedule, consider assignment approvals
          schedule.getAssignmentFor(ds) match {
            case Some(assn) => literalsWithAssignment(slot, ds, assn)
            case None => literalsNoAssignment(slot, ds)
          }
        case None =>
          // no old schedule, just use availability
          literalsNoAssignment(slot, ds)
      }

      val expr = if (literals.nonEmpty) { Or(literals) } else { Equals(NumeralLit(1), NumeralLit(1)) /* nop */ }
      Assert(expr)
    }.toList

    assertions
  }

  def definition : List[Command] = List()
  def asserts: List[Assert] = assertions
}