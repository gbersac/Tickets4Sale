package ticket4Sale

import scala.util.Try
import scala.util.Success
import scala.io.Source

/** Given that:
  * 1- this is the only use of json in the application
  * 2- The structure of this json is nothing like other structures in the application
  * I decided to write this json directly rather than implementing a json reader & writer typeclass like I would have
  * done in a normal production application.
  */
object ShowsJSONFormatter {
  import ujson._
  def apply(shows: List[ShowAtDate]): Obj = {
      val showsByGenre: Seq[Obj] =
        Map(
          "drama" -> shows.filter(_.description.genre == Drama),
          "comedy" -> shows.filter(_.description.genre == Comedy),
          "musical" -> shows.filter(_.description.genre == Musical),
          )
        .filter(!_._2.isEmpty)
        .map { case (genre, showsG) =>
          Obj(
            "genre" -> genre.toString().toLowerCase(),
            "shows" -> showsG.map(s =>
              Obj(
                "title" -> s.description.title.toLowerCase(),
                // number are formatted as string in the inscrution examples!
                "tickets_left" -> s.ticketsLeft.toString(),
                "tickets_available" -> s.ticketsAvailable.toString(),
                "status" -> ShowStatus.toString(s.status)
              )
            )
          )
        }
      .toList
    Obj("inventory" -> showsByGenre.reverse)
  }
}

object Ticket4Sale {

  def main(args: Array[String]): Unit = {
    if (args.length < 3)
      println("usage: ./tickets4Sale path query-date show-date")
    val path = args(0)
    for {

      queryDate <- Date(args(1)).toRight(s"Date format incorrect ${args(1)}")
      showDate <- Date(args(2)).toRight(s"Date format incorrect ${args(2)}")

      csvString <- Try(Source.fromFile(path)) match {
        case Success(s) => Right(s.getLines.mkString)
        case _ => Left(s"Cannot open the file $path")
      }

      showInventory <- ShowInventory.fromCSV(csvString) match {
        case Right(value) => Right(value)
        case Left(err) => Left(s"CSV parsing failed: ${err.mkString(" # ")}")
      }

    } yield showInventory.query(showDate, queryDate)
  }

}
