package Core

import java.time.LocalDate

case class Day(date: LocalDate, slots: Set[Timeslot]) {
  def dateslots = slots.map(_.toDateslot(date))
}

