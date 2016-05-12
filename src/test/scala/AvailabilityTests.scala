import java.time.{LocalDate, LocalDateTime, LocalTime, Month}

import Core.{Availability, Dateslot, Person}
import org.scalatest.{FlatSpec, Matchers}

class AvailabilityTests extends FlatSpec with Matchers {
  "A person" should "be available if they are actually available" in {
    val date = LocalDate.of(2016, Month.MAY, 12)

    val a = Availability.AllDay(2016, Month.MAY, 12)
    val p = Person("Jane", "Doe", a)

    val d = Dateslot("meeting",
                     LocalDateTime.of(date, LocalTime.of(11, 15)),
                     LocalDateTime.of(date, LocalTime.of(12, 15))
            )

    p.availableFor(d) should be (true)
  }

  "A person" should "not be available if they are not available" in {
    val date = LocalDate.of(2016, Month.MAY, 12)

    val a = Availability.NotAvailable
    val p = Person("Jane", "Doe", a)

    val d = Dateslot("meeting",
      LocalDateTime.of(date, LocalTime.of(11, 15)),
      LocalDateTime.of(date, LocalTime.of(12, 15))
    )

    p.availableFor(d) should be (false)
  }

  "A person with schedule exceptions" should "not be available if they are not available" in {
    val date = LocalDate.of(2016, Month.MAY, 12)
    val except = Set(
      (LocalDateTime.of(date, LocalTime.of(11,15)),LocalDateTime.of(date, LocalTime.of(12,15)))
    )

    val a = Availability.AllDayExcept(2016, Month.MAY, 12, except)
    val p = Person("Jane", "Doe", a)

    val d = Dateslot("meeting",
      LocalDateTime.of(date, LocalTime.of(11, 15)),
      LocalDateTime.of(date, LocalTime.of(12, 15))
    )

    p.availableFor(d) should be (false)
  }

  "A person with schedule exceptions" should "be available if they are available" in {
    val date = LocalDate.of(2016, Month.MAY, 12)
    val except = Set(
      (LocalDateTime.of(date, LocalTime.of(11,15)),LocalDateTime.of(date, LocalTime.of(12,15)))
    )

    val a = Availability.AllDayExcept(2016, Month.MAY, 12, except)
    val p = Person("Jane", "Doe", a)

    val d = Dateslot("meeting",
      LocalDateTime.of(date, LocalTime.of(13, 15)),
      LocalDateTime.of(date, LocalTime.of(14, 15))
    )

    p.availableFor(d) should be (true)
  }
}
