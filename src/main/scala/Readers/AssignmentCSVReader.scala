package Readers

import java.io.File
import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeFormatter

import Core._
import com.github.tototoshi.csv.CSVReader
import edu.umass.cs.smtlib.SMT

case class AssignmentCSVReader(filename: String) {
  private val reader = CSVReader.open(new File(filename))
  private val raw: List[Map[String, String]] = reader.allWithHeaders()
  private var keys = Map[String,Int]()

  private def munge(key: String) : String = key.replaceAll("[^A-Za-z0-9]", "")

  private def dayFromRows(datestring: String, rows: List[Map[String,String]]) : Day = {
    val dateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy")
    val timeFormatter = DateTimeFormatter.ofPattern("H:m")

    val date = LocalDate.parse(datestring, dateFormatter)

    val slots = rows.flatMap { case row =>
      if (row("approval").nonEmpty) {
        None
      } else {
        // get canonical name
        val event = munge(row("event"))

        // add an index to make unique key
        if (keys.contains(event)) {
          keys += event -> (keys(event) + 1)
        } else {
          keys += event -> 0
        }

        // output slot with unique name (the key)
        Some(
          Timeslot(
            event + "_" + keys(event),
            row("event"),
            row("role"),
            LocalTime.parse(row("start"), timeFormatter),
            LocalTime.parse(row("end"), timeFormatter)
          )
        )
      }
    }.toSet

    Day(date, slots)
  }

  private def assignmentsFromRows(datestring: String, rows: List[Map[String,String]], people: Set[Person], defaultPerson: Person) : List[Assignment] = {
    val dateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy")
    val timeFormatter = DateTimeFormatter.ofPattern("H:m")

    val date = LocalDate.parse(datestring, dateFormatter)

    rows.flatMap { case row =>
      // get canonical name
      val event = munge(row("event"))

      // add an index to make unique key
      if (keys.contains(event)) {
        keys += event -> (keys(event) + 1)
      } else {
        keys += event -> 0
      }

      // find person
      val person: Person =
        if (row("person").nonEmpty) {
          try {
            people.find { p => p.canonicalName == row("person") }.get
          } catch {
            case e:Exception =>
              throw new Exception("ERROR: Schedulous does not know person: " + row("person"))
          }
        } else {
          defaultPerson
        }

      // find approval
      val approval = Approval.fromString(row("approval"))

      // get slot with unique name (the key)
      val ds = Timeslot(
        event + "_" + keys(event),
        row("event"),
        row("role"),
        LocalTime.parse(row("start"), timeFormatter),
        LocalTime.parse(row("end"), timeFormatter)
      ).toDateslot(date)

      Some(
        Assignment(
          SMT.freshName(ds.z3name),
          ds,
          person,
          approval
        )
      )
    }
  }

  def days : Set[Day] = {
    val rowsByDay: Map[String, List[Map[String, String]]] = raw.groupBy(row => row("date"))

    rowsByDay.map { case (datestring, rows) => dayFromRows(datestring, rows) }.toSet
  }

  def assignments(people: Set[Person]): (Schedule,Schedule) = {
    val rowsByDay: Map[String, List[Map[String, String]]] = raw.groupBy(row => row("date"))

    val defaultPerson = Person("Jane", "Doe", Availability.NotAvailable)

    val assns: List[Assignment] = rowsByDay.flatMap { case (datestring, rows) => assignmentsFromRows(datestring, rows, people, defaultPerson) }.toList

    val (unfilled,filled) = assns.partition ( assn => assn.person == defaultPerson )

    (Schedule(unfilled),Schedule(filled))
  }
}
