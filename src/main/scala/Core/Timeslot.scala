package Core

import java.sql.Timestamp
import java.time.{Duration, LocalDateTime, LocalTime}

case class Timeslot(z3name: String, prettyname: String, start: LocalTime, end: LocalTime) { assert(end.isAfter(start)) }
case class Dateslot(z3name: String, prettyname: String, start: LocalDateTime, end: LocalDateTime) {
  def overlaps(other: Dateslot) : Boolean = {
    dateBetween(this,other) || dateBetween(other, this)
  }
  private def dateBetween(date: Dateslot, otherdate: Dateslot) : Boolean = {
    (date.start.isEqual(otherdate.start) || date.start.isAfter(otherdate.start)) &&
      date.start.isBefore(otherdate.end)
  }

  def duration : Duration = Duration.between(start, end)

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case ds: Dateslot =>
        this.z3name == ds.z3name &&
        this.start.isEqual(ds.start) &&
        this.end.isEqual(ds.end)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val start_ts = Timestamp.valueOf(start)
    val end_ts = Timestamp.valueOf(end)
    ((start_ts.getTime + end_ts.getTime) % Int.MaxValue).toInt
  }
}
