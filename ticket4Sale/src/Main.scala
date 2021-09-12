package ticket4Sale

import scala.util.Try
import scala.util.Success
import scala.io.Source

/** Given that:
  * 1- this is the only use of json in the application
  * 2- The structure of this json is nothing like other structures in the application
  * It would have been counter productive to implement a json reader & writer typeclass for a new case class created
  * specifically to match the expected structure of the output json. I decided to create the json like this instead:
  */
object ShowsJSONFormatter {
  import ujson._
  def apply(shows: Seq[ShowAtDate]): Obj = {
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
    val result = for {

      path <- if (args.length < 3) Left("usage: ./tickets4Sale path query-date show-date") else Right(args(0))

      queryDate <- Date(args(1)).toRight(s"Date format incorrect ${args(1)}")
      showDate <- Date(args(2)).toRight(s"Date format incorrect ${args(2)}")

      csvString <- Try(Source.fromFile(path)) match {
        case Success(s) => Right(s.getLines.mkString("\n"))
        case _ => Left(s"Cannot open the file $path")
      }

      showInventory <- ShowInventory.fromCSV(csvString) match {
        case Right(value) => Right(value)
        case Left(err) => Left(s"CSV parsing failed: ${err.mkString(" # ")}")
      }

    } yield ShowsJSONFormatter(showInventory.query(queryDate, showDate))

    result match {
      case Left(err) => Console.err.println(err)
      case Right(value) => println(value)
    }
  }

}
