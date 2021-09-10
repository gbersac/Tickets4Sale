package ticket4Sale

import java.time.LocalDate

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

final case class Show(title: String, openingDay: LocalDate, genre: Genre)

final case class ShowInventory(shows: List[Show])

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
