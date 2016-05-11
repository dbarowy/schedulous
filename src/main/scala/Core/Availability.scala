package Core

import java.time.{LocalDate, LocalTime, Month, LocalDateTime}

object Availability {
  type Span = (LocalDateTime,LocalDateTime)

  def NotAvailable : Availability = {
    Availability(Set.empty)
  }
  def AllDay(day: LocalDate) : Availability = {
    val start = LocalDateTime.of(day, LocalTime.MIN)
    val end = LocalDateTime.of(day, LocalTime.MAX)
    Availability(Set((start,end)))
  }
  def AllDay(year: Int, month: Month, dayOfMonth: Int) : Availability = {
    val day = LocalDate.of(year, month, dayOfMonth)
    AllDay(day)
  }
  def AllDayExcept(year: Int, month: Month, dayOfMonth: Int, except: Set[Span]) : Availability = {
    val day = LocalDate.of(year, month, dayOfMonth)
    val start = LocalDateTime.of(day, LocalTime.MIN)
    val end = LocalDateTime.of(day, LocalTime.MAX)
    val whole_day: Option[Span] = Some(start,end)

    val (_,slots: List[Span]) =
      except.foldLeft(whole_day,List[Span]()) {
        case ((todo_opt: Option[Span], cut_spans: List[Span]), not_at_time) =>
        todo_opt match {
          case Some(todo: Span) =>
            difference(todo, not_at_time) match {
              case (Some(done: Span),Some(to_cut: Span)) => (Some(to_cut), done :: cut_spans)
              case (Some(done: Span),None) => (None, done :: cut_spans)
              case (None,Some(to_cut: Span)) => (Some(to_cut), cut_spans)
              case (None,None) => (None,cut_spans)
            }
          case _ => (None,cut_spans)
        }
      }

    Availability(slots.toSet)
  }

  /**
    * Subtracts span2 from span1.
    * @param span1
    * @param span2
    * @return Set difference.
    */
  private def difference(span1: Span, span2: Span) : (Option[Span],Option[Span]) = {
    val (start1,end1) = span1
    val (start2,end2) = span2

    val left = if (end1.isBefore(start2)) {
      // left uncut
      Some(start1,end1)
    } else if (start1.isBefore(start2)) {
      // left cut
      Some(start1,start2)
    } else {
      // left empty
      None
    }

    val right = if (!start1.isBefore(end2)) {
      // right uncut
      Some(start1,end1)
    } else if (end2.isBefore(end1)) {
      // right cut
      Some(end2,end1)
    } else {
      // right empty
      None
    }

    (left,right)
  }
}

case class Availability(schedule: Set[(LocalDateTime,LocalDateTime)]) {
  def +(a2: Availability) : Availability = Availability(this.schedule.union(a2.schedule))
}
