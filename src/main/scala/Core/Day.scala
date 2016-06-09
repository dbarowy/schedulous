package Core

import java.time.{LocalDateTime, LocalDate}

case class Day(date: LocalDate, slots: Set[Timeslot]) {
  def dateslots = slots.map { slot =>
    Dateslot(
      slot.z3name,
      slot.prettyname,
      LocalDateTime.of(date, slot.start),
      LocalDateTime.of(date, slot.end)
    )
  }
}

