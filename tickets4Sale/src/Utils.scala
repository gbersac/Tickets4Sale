package tickets4Sale

import scala.util.Try
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import zamblauskas.csv.parser.ColumnReads
import zamblauskas.csv.parser.Reads

object Int {
  def unapply(s: String): Option[Int] = Try(s.toInt).toOption
}

object Date {
  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-M-dd")

  import zamblauskas.csv.parser._
  implicit val csvDateRead: Reads[LocalDate] = new Reads[LocalDate] {
    def read(column: Column): ReadResult[LocalDate] = Try[ReadResult[LocalDate]] {
      Date(column.value) match {
        case Some(value) => ReadSuccess(value)
        case None => ReadFailure("Incorrect format. Expected yyyy-M-dd")
      }
    }.getOrElse {
      ReadFailure("Cannot convert to date")
    }
  }

  def unapply(s: String): Option[LocalDate] = Try(LocalDate.parse(s, formatter)).toOption
  def apply(s: String): Option[LocalDate] = Try(LocalDate.parse(s, formatter)).toOption
}
