package Core

import smtlib.parser.Terms.SSymbol

object Assignment {
  var counter = 0
  def nextID() = {
    val c = counter
    counter += 1
    c
  }
  def apply(slotname: SSymbol, slot: Dateslot, person: Person, approval: Approval) : Assignment = {
    val id = Assignment.nextID()
    Assignment(id, slotname, slot, person, approval)
  }
}

case class Assignment(id: Int, slotname: SSymbol, slot: Dateslot, person: Person, approval: Approval) {
  def approve() : Assignment = Assignment(id, slotname, slot, person, Approved)
  def reject() : Assignment = Assignment(id, slotname, slot, person, Rejected)
  def undecided() : Assignment = this
}
