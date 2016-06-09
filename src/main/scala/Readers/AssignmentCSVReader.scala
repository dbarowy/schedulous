package Readers

import java.io.File
import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeFormatter
import Core.{Day, Timeslot}
import com.github.tototoshi.csv.CSVReader

case class AssignmentCSVReader(filename: String) {
  private val reader = CSVReader.open(new File(filename))
  private val raw: List[Map[String, String]] = reader.allWithHeaders()
  private var keys = Map[String,Int]()

  private def munge(key: String) : String = key.replaceAll("[^A-Za-z0-9]", "")

  private def dayFromRows(datestring: String, rows: List[Map[String,String]]) : Day = {
    val dateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy")
    val timeFormatter = DateTimeFormatter.ofPattern("H:m")

    val date = LocalDate.parse(datestring, dateFormatter)

    val slots = rows.map { case row =>
      // get canonical name
      val event = munge(row("event"))

      // add an index to make unique key
      if (keys.contains(event)) {
        keys += event -> (keys(event) + 1)
      } else {
        keys += event -> 0
      }

      // output slot with unique name (the key)
      Timeslot(
        event + "_" + keys(event),
        row("event"),
        LocalTime.parse(row("start"), timeFormatter),
        LocalTime.parse(row("end"), timeFormatter)
      )
    }.toSet

    Day(date, slots)
  }

  def days : Set[Day] = {
    val rowsByDay: Map[String, List[Map[String, String]]] = raw.groupBy(row => row("date"))

    rowsByDay.map { case (datestring, rows) => dayFromRows(datestring, rows) }.toSet
  }
}
