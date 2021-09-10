package ticket4Sale

import scala.util.Try
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Int {
  def unapply(s: String): Option[Int] = Try(s.toInt).toOption
}

object Date {
  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-M-dd")

  def unapply(s: String): Option[LocalDate] = Try(LocalDate.parse(s, formatter)).toOption
  def apply(s: String): Option[LocalDate] = Try(LocalDate.parse(s, formatter)).toOption
}
