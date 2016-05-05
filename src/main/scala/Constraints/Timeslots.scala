package Constraints

import Core.{Dateslot, Day}
import edu.umass.cs.smtlib.SMT
import smtlib.parser.Commands.{Assert, DeclareConst, Command}
import smtlib.parser.Terms.SSymbol
import smtlib.theories.Ints.IntSort

case class Timeslots(days: Set[Day]) extends Constraint {
  type SlotMap = Map[SSymbol,Dateslot]

  private val slotmap = days.flatMap { day =>
    day.dateslots.map { slot =>
      SMT.freshName(slot.eventname) -> slot
    }
  }.toMap

  val defns =
    slotmap
      .toList
      .sortWith { case ((_,slot1),(_,slot2)) => slot1.start.isBefore(slot2.start) }
      .map { case (symb,_) => DeclareConst(symb, IntSort())}

  def asserts: List[Assert] = List.empty
  def definition: List[Command] = defns
  def slotMap = slotmap
}
