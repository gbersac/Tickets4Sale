package ticket4Sale

import java.time.LocalDate
import java.time.temporal.ChronoUnit

sealed trait Genre { val price: Int }
final case object Musical extends Genre { val price: Int = 70}
final case object Comedy extends Genre { val price: Int = 50}
final case object Drama extends Genre { val price: Int = 40}

object Genre {
  def unapply(s: String): Option[Genre] = s match {
    case "musicals" | "musical" => Some(Musical)
    case "comedy" => Some(Comedy)
    case "drama" => Some(Drama)
    case _ => None
  }

}

sealed trait ShowStatus
case object SaleNotStarted extends ShowStatus
case object OpenForSale extends ShowStatus
case object SoldOut extends ShowStatus
case object InThePast extends ShowStatus

final case class ShowAtDate(
  description: Show,
  showDate: LocalDate,
  queryDate: LocalDate,
) {
  /** It will be the `representationNumber`th occurence of this show on the date `show date` */
  val representationNumber: Long = description.openingDay.until(showDate, ChronoUnit.DAYS) + 1
  val daysBeforeGoingToTheShow: Long = queryDate.until(showDate, ChronoUnit.DAYS)

  val ticketsAvailable =
    if (representationNumber < 0 || representationNumber > 100) 0
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

final case class ShowInventory(shows: List[Show]) {
  // If queryDate > showDate
  def query(showDate: LocalDate, queryDate: LocalDate): List[ShowAtDate] =
    shows.filter(s => s.openingDay.isBefore(showDate) || s.openingDay.isEqual(showDate))
      .map(_.stateAtDate(showDate, queryDate))
}

object ShowInventory {

  def fromCSV(s: String): Either[List[String], ShowInventory] = {
    val lines = s.split("\n")
      .filter(_ != "")
      .map(s =>
        s.split(",").toList match {
          case s :: Date(d) :: Genre(x) :: Nil => Right(Show(s, d, x))
          case _ => Left(s)
        }
      )
    lines.partitionMap(identity) match {
      case (Array(), rights) => Right(ShowInventory(rights.toList))
      case (lefts, _)    => Left(lefts.toList)
    }
  }
}
