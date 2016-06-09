package Core

case class Person(fname: String, lname: String, availability: Availability) {
  def availableFor(dateslot: Dateslot) : Boolean = {
    availability.schedule.foldLeft(false){ case (avail,(start,end)) =>
        avail || (!dateslot.start.isBefore(start) && !dateslot.end.isAfter(end))
    }
  }

  def canonicalName: String = (fname + lname).replaceAll("[^a-zA-Z]", "")

  override def hashCode(): Int = {
    ((this.fname.hashCode.toLong + this.lname.hashCode.toLong) % Int.MaxValue).toInt
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case p: Person =>
        this.fname == p.fname && this.lname == p.lname
      case _ => false
    }
  }

  override def toString: String = fname + " " + lname
}
