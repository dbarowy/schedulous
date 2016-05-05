import java.time.{Month, LocalTime, LocalDate}
import Core._

object Schedulous extends App {
  // schedule
  val Sunday = Day(
    LocalDate.of(2016, Month.JUNE, 12),
    Set(
      Timeslot("SEXYPL", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("DUMBPL", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("SEXYPL", LocalTime.of(10,0), LocalTime.of(11,15)),
      Timeslot("POOPL", LocalTime.of(10,0), LocalTime.of(11,15))
    )
  )
  val Monday = Day(
    LocalDate.of(2016, Month.JUNE, 13),
    Set(
      Timeslot("TRACK1.1", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("TRACK2.1", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("TRACK1.2", LocalTime.of(10,15), LocalTime.of(11,15)),
      Timeslot("TRACK2.2", LocalTime.of(10,15), LocalTime.of(11,15))
    )
  )

  // people
  val people = Set(
    Person("Dan", "Barowy"),
    Person("Michael", "Christensen"),
    Person("Emma", "Tosch"),
    Person("John", "Vilk"),
    Person("Bobby", "Powers"),
    Person("Rian", "Shambaugh")
  )

  // days
  val days = Set(Sunday, Monday)

  val schedule = Schedule.solve(people, days)

  // get schedule
  println(schedule)
}