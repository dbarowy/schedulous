package Core

object Assignment {
  var counter = 0
  def nextID() = {
    val c = counter
    counter += 1
    c
  }
  def apply(slot: Dateslot, person: Person, approval: Approval) : Assignment = {
    val id = Assignment.nextID()
    Assignment(id, slot, person, approval)
  }
}

case class Assignment(id: Int, slot: Dateslot, person: Person, approval: Approval) {
  def approve() : Assignment = Assignment(id, slot, person, Approved)
  def reject() : Assignment = Assignment(id, slot, person, Rejected)
  def undecided() : Assignment = this
}
