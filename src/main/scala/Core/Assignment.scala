package Core

import java.util.UUID
import smtlib.parser.Terms.SSymbol

object Assignment {
  def apply(slotname: SSymbol, slot: Dateslot, person: Person, approval: Approval) : Assignment = {
    val id = UUID.randomUUID()
    Assignment(id, slotname, slot, person, approval)
  }
  def header: String =
    "\"start\",\"end\",\"event\",\"role\",\"person\",\"approval\""
}

case class Assignment(id: UUID, slotname: SSymbol, slot: Dateslot, person: Person, approval: Approval) {
  def approve() : Assignment = Assignment(id, slotname, slot, person, Approved)
  def reject() : Assignment = Assignment(id, slotname, slot, person, Rejected)
  def undecided() : Assignment = this

  override def toString: String =
    "\"" + slot.start +
    "\",\"" + slot.end +
    "\",\"" + slot.prettyname +
    "\",\"" + slot.role +
    "\",\"" + person.fname + " " + person.lname +
    "\",\"" + approval + "\""
}
