package ticket4Sale

import utest._
import java.time.LocalDate

object ShowInventoryParsingTests extends TestSuite {
  val tests = Tests {
    test("with the instruction example") {
      val s = """
             |Cats,2018-06-01,musical
             |Comedy of Errors,2018-07-01,comedy
             |Everyman,2018-08-01,drama
             |""".stripMargin
      val expectedResult = ShowInventory(List(
        Show("Cats", Date("2018-06-01").get, Musical),
        Show("Comedy of Errors", Date("2018-07-01").get, Comedy),
        Show("Everyman", Date("2018-08-01").get, Drama),
      ))
      assert(ShowInventory.fromCSV(s) == Right(expectedResult))
    }

    test("with an empty file") {
      assert(ShowInventory.fromCSV("") == Right(ShowInventory(List())))
    }

    test("with an erroneous file") {
      val s = """
             |Cats,2018-06-0,musical
             |Comedy of Errors,2018-07-01
             |Everyman,2018-08-01,drama
             |""".stripMargin
      val expectedResult = Left(List("Cats,2018-06-0,musical", "Comedy of Errors,2018-07-01"))
      assert(ShowInventory.fromCSV(s) == expectedResult)
    }
  }
}
