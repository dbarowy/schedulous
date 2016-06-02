package Constraints

import Core.{Dateslot, Day}
import edu.umass.cs.smtlib.SMT
import smtlib.parser.Commands.{Assert, DeclareConst, Command}
import smtlib.parser.Terms.SSymbol
import smtlib.theories.Ints.IntSort

object Timeslots {
  def apply(days: Set[Day]) = new Timeslots(days)
  def apply(days: Set[Day], slotmap: Map[SSymbol,Dateslot]) = new Timeslots(days, slotmap)
}

class Timeslots(days: Set[Day], slotmap: Map[SSymbol,Dateslot]) extends Constraint {
  type SlotMap = Map[SSymbol,Dateslot]

  def this(days: Set[Day]) = {
    this(
      days,
      days.flatMap { day =>
        day.dateslots.map { slot =>
          SMT.freshName(slot.eventname) -> slot
        }
      }.toMap
    )
  }

  val defns =
    slotmap
      .toList
      .sortWith { case ((_,slot1),(_,slot2)) => slot1.start.isBefore(slot2.start) }
      .map { case (symb,_) => DeclareConst(symb, IntSort())}

  def asserts: List[Assert] = List.empty
  def definition: List[Command] = defns
  def slotMap = slotmap
}
