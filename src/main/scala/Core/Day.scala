package Core

import java.time.{LocalTime, Month, LocalDateTime, LocalDate}

case class Day(date: LocalDate, slots: Set[Timeslot]) {
  def dateslots = slots.map { slot =>
    Dateslot(
      slot.eventname,
      LocalDateTime.of(date, slot.start),
      LocalDateTime.of(date, slot.end)
    )
  }
}

