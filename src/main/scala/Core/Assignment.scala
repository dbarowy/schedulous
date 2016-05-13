package Core

object Assignment {
  var counter = 0
  def nextID() = {
    val c = counter
    counter += 1
    c
  }
}

case class Assignment(ds: Dateslot, p: Person, a: Approval) {
  val id = Assignment.nextID()
}
