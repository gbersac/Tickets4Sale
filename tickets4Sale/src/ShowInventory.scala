package tickets4Sale

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scala.util.Try

sealed trait Genre { val price: Int }
final case object Musical extends Genre { val price: Int = 70}
final case object Comedy extends Genre { val price: Int = 50}
final case object Drama extends Genre { val price: Int = 40}

object Genre {
  def unapply(s: String): Option[Genre] = s.toLowerCase match {
    case "musicals" | "musical" => Some(Musical)
    case "comedy" => Some(Comedy)
    case "drama" => Some(Drama)
    case _ => None
  }

  import zamblauskas.csv.parser._
  implicit val csvGenreRead: Reads[Genre] = new Reads[Genre] {
    def read(column: Column): ReadResult[Genre] = Try[ReadResult[Genre]] {
      Genre.unapply(column.value) match {
        case Some(value) => ReadSuccess(value)
        case None => ReadFailure("Incorrect format. Expected yyyy-M-dd")
      }
    }.getOrElse {
      ReadFailure("Cannot convert to date")
    }
  }

}

sealed trait ShowStatus
case object SaleNotStarted extends ShowStatus
case object OpenForSale extends ShowStatus
case object SoldOut extends ShowStatus
case object InThePast extends ShowStatus

object ShowStatus {
  def toString(s: ShowStatus): String = s match {
    case InThePast => "in the past"
    case OpenForSale => "open for sale"
    case SaleNotStarted => "sale not started"
    case SoldOut => "sold out"
  }
}

final case class ShowAtDate(
  description: Show,
  showDate: LocalDate,
  queryDate: LocalDate,
) {
  /** It will be the `representationNumber`th occurence of this show on the date `show date` */
  val representationNumber: Long = description.openingDay.until(showDate, ChronoUnit.DAYS) + 1
  val daysBeforeGoingToTheShow: Long = queryDate.until(showDate, ChronoUnit.DAYS)

  val ticketsAvailable: Int =
    if (representationNumber < 0 || representationNumber > 100) 0
    else if (daysBeforeGoingToTheShow < 5 || daysBeforeGoingToTheShow >= 25) 0
    else if (representationNumber > 60) 5
    else 10

  val totalPlaces = if (representationNumber > 60) 100 else 200 // sold places + unsold places

  val ticketsLeft =
    if (representationNumber < 0 || representationNumber > 100) 0
    else if (daysBeforeGoingToTheShow < 5) 0
    else if (daysBeforeGoingToTheShow >= 25) totalPlaces
    else (daysBeforeGoingToTheShow - 4) * ticketsAvailable

  val status =
    if (daysBeforeGoingToTheShow >= 25) SaleNotStarted
    else if (daysBeforeGoingToTheShow < 0) InThePast
    else if (daysBeforeGoingToTheShow < 5) SoldOut
    else OpenForSale
}

final case class Show(title: String, openingDay: LocalDate, genre: Genre) {
  def stateAtDate(showDate: LocalDate, queryDate: LocalDate): ShowAtDate = ShowAtDate(this, showDate, queryDate)
}

final case class ShowInventory(shows: Seq[Show]) {
  // If queryDate > showDate
  def query(showDate: LocalDate, queryDate: LocalDate): Seq[ShowAtDate] =
    shows.filter(s => s.openingDay.isBefore(showDate) || s.openingDay.isEqual(showDate))
      .map(_.stateAtDate(showDate, queryDate))
}

object ShowInventory {
  import zamblauskas.csv.parser._
  import Date.csvDateRead

  private val csvHeader = "title,openingDay,genre"
  def fromCSV(s: String): Either[String, ShowInventory] = {
    val s2 = if (s.startsWith(csvHeader)) s else csvHeader + "\n" + s
    Parser.parse[Show](s2) match {
      case Right(lines) => Right(ShowInventory(lines))
      case Left(f) => Left(s"Error on line ${f.lineNum}: ${f.message}")
    }
  }
}
