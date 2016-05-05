package Core

import java.time.{LocalTime, LocalDateTime}

case class Timeslot(eventname: String, start: LocalTime, end: LocalTime) { assert(end.isAfter(start)) }
case class Dateslot(eventname: String, start: LocalDateTime, end: LocalDateTime) {
  def overlaps(other: Dateslot) : Boolean = {
    dateBetween(this,other) || dateBetween(other, this)
  }
  private def dateBetween(date: Dateslot, otherdate: Dateslot) : Boolean = {
    (date.start.isEqual(otherdate.start) || date.start.isAfter(otherdate.start)) &&
      date.start.isBefore(otherdate.end)
  }
}
