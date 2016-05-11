package Core

case class Person(fname: String, lname: String, availability: Availability) {
  def availableFor(dateslot: Dateslot) : Boolean = {
    availability.schedule.foldLeft(false){ case (avail,(start,end)) =>
        avail || (!dateslot.start.isBefore(start) && !dateslot.end.isAfter(end))
    }
  }
}
