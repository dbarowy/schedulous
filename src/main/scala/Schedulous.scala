import java.time.{Month, LocalTime, LocalDate}
import Core._

object Schedulous extends App {
  // schedule
  val Sunday = LocalDate.of(2016, Month.JUNE, 12)
  val Monday = LocalDate.of(2016, Month.JUNE, 13)

  val SundayEvents = Day(
    Sunday,
    Set(
      Timeslot("SEXYPL", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("DUMBPL", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("SEXYPL", LocalTime.of(10,0), LocalTime.of(11,15)),
      Timeslot("POOPL", LocalTime.of(10,0), LocalTime.of(11,15))
    )
  )
  val MondayEvents = Day(
    Monday,
    Set(
      Timeslot("TRACK1.1", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("TRACK2.1", LocalTime.of(9,0), LocalTime.of(10,0)),
      Timeslot("TRACK1.2", LocalTime.of(10,15), LocalTime.of(11,15)),
      Timeslot("TRACK2.2", LocalTime.of(10,15), LocalTime.of(11,15))
    )
  )

  // people
  val people = Set(
    Person("Dan", "Barowy", Availability.AllDay(Sunday)),
    Person("Michael", "Christensen", Availability.AllDay(Monday) + Availability.AllDay(Sunday)),
    Person("Emma", "Tosch", Availability.AllDay(Monday) + Availability.AllDay(Sunday)),
    Person("John", "Vilk", Availability.AllDay(Monday) + Availability.AllDay(Sunday)),
    Person("Bobby", "Powers", Availability.AllDay(Monday) + Availability.AllDay(Sunday)),
    Person("Rian", "Shambaugh", Availability.AllDay(Monday) + Availability.AllDay(Sunday))
  )

  // days
  val events = Set(SundayEvents, MondayEvents)

  val schedule = Schedule.solve(people, events)

  // get schedule
  schedule match {
    case Some(s) => println(s)
    case None => "Cannot find schedule that meets constraints."
  }
}