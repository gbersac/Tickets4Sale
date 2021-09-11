package ticket4Sale

import utest._
import java.time.LocalDate

object ShowInventoryParsingTests extends TestSuite {
  val tests = Tests { test("CSV parsing") {
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
  }}
}

object ShowAtDateTests extends TestSuite {
  val showInventoryC = ShowInventory.fromCSV("custom,2018-08-30,drama").getOrElse(throw new UnknownError)
  val showInventoryCats = ShowInventory.fromCSV("Cats,2018-06-01,musical").getOrElse(throw new UnknownError)
  val showInventoryCOE = ShowInventory.fromCSV("COE,2018-07-01,comedy").getOrElse(throw new UnknownError)
  val showInventoryEveryman = ShowInventory.fromCSV("Everyman,2018-08-01,drama").getOrElse(throw new UnknownError)

  val jan01 = Date.apply("2018-01-01").get
  val jul10 = Date.apply("2018-07-10").get
  val jul11 = Date.apply("2018-07-11").get
  val jul12 = Date.apply("2018-07-12").get
  val jul13 = Date.apply("2018-07-13").get
  val jul01 = Date.apply("2018-07-01").get
  val jul30 = Date.apply("2018-07-30").get
  val jul31 = Date.apply("2018-07-31").get
  val aug01 = Date.apply("2018-08-01").get
  val aug02 = Date.apply("2018-08-02").get
  val aug04 = Date.apply("2018-08-04").get
  val aug05 = Date.apply("2018-08-05").get
  val aug06 = Date.apply("2018-08-06").get
  val aug15 = Date.apply("2018-08-15").get
  val aug25 = Date.apply("2018-08-25").get
  val aug26 = Date.apply("2018-08-26").get
  val aug30 = Date.apply("2018-08-30").get
  val aug31 = Date.apply("2018-08-31").get
  val sep01 = Date.apply("2018-09-01").get
  val sep15 = Date.apply("2018-09-15").get
  val sep30 = Date.apply("2018-09-30").get
  val oct15 = Date.apply("2018-10-15").get
  val oct28 = Date.apply("2018-10-28").get // last day in the big hall
  val oct29 = Date.apply("2018-10-29").get // first day in the small hall
  val dec07 = Date.apply("2018-12-07").get // last representation day
  val dec08 = Date.apply("2018-12-08").get // first day after the end of the show

  def query(showDate: LocalDate, queryDate: LocalDate, si: ShowInventory): ShowAtDate =
    si.query(showDate, queryDate).head
  def queryC(showDate: LocalDate, queryDate: LocalDate): ShowAtDate =
    query(showDate, queryDate, showInventoryC)

    val tests = Tests { test("Test ShowAtDate") {
    test("representationNumber") {
      queryC(aug30, aug01).representationNumber ==> 1
      queryC(sep01, aug01).representationNumber ==> 3
      queryC(sep15, aug01).representationNumber ==> 17
      queryC(oct15, aug01).representationNumber ==> 47
      queryC(oct28, aug01).representationNumber ==> 60
      queryC(oct29, aug01).representationNumber ==> 61
      queryC(dec07, aug01).representationNumber ==> 100
      queryC(dec08, aug01).representationNumber ==> 101
    }

    test("daysBeforeGoingToTheShow") {
      query(aug05, jul10, showInventoryCats).daysBeforeGoingToTheShow ==> 26
      query(aug05, jul11, showInventoryCats).daysBeforeGoingToTheShow ==> 25
      query(aug05, jul12, showInventoryCats).daysBeforeGoingToTheShow ==> 24
      query(aug05, jul13, showInventoryCats).daysBeforeGoingToTheShow ==> 23
      query(aug05, jul30, showInventoryCats).daysBeforeGoingToTheShow ==> 6
      query(aug05, jul31, showInventoryCats).daysBeforeGoingToTheShow ==> 5
      query(aug05, aug01, showInventoryCats).daysBeforeGoingToTheShow ==> 4
      query(aug05, aug02, showInventoryCats).daysBeforeGoingToTheShow ==> 3
      query(aug05, aug05, showInventoryCats).daysBeforeGoingToTheShow ==> 0
      query(aug05, aug06, showInventoryCats).daysBeforeGoingToTheShow ==> -1
    }

    test("ticketsLeft") {
      // instruction example 1
      query(jul01, jan01, showInventoryCats).ticketsLeft ==> 200
      query(jul01, jan01, showInventoryCOE).ticketsLeft ==> 200

      // instruction example 2
      query(aug15, aug01, showInventoryCOE).ticketsLeft ==> 100
      query(aug15, aug01, showInventoryEveryman).ticketsLeft ==> 100
      query(aug15, aug01, showInventoryCats).ticketsLeft ==> 50

      // custom tests
      query(aug05, jul10, showInventoryCats).ticketsLeft ==> 100
      query(aug05, jul11, showInventoryCats).ticketsLeft ==> 100
      query(aug05, jul12, showInventoryCats).ticketsLeft ==> 100
      query(aug05, jul13, showInventoryCats).ticketsLeft ==> 95
      query(aug05, jul30, showInventoryCats).ticketsLeft ==> 10
      query(aug05, jul31, showInventoryCats).ticketsLeft ==> 5
      query(aug05, aug01, showInventoryCats).ticketsLeft ==> 0
      query(aug05, aug02, showInventoryCats).ticketsLeft ==> 0
      query(aug05, aug05, showInventoryCats).ticketsLeft ==> 0
      query(aug05, aug06, showInventoryCats).ticketsLeft ==> 0
    }

    test("status") {
      // instruction example 1
      query(jul01, jan01, showInventoryCats).status ==> SaleNotStarted
      query(jul01, jan01, showInventoryCOE).status ==> SaleNotStarted
      showInventoryEveryman.query(jul01, jan01).isEmpty ==> true

      // instruction example 2
      query(aug15, aug01, showInventoryCats).status ==> OpenForSale
      query(aug15, aug01, showInventoryCOE).status ==> OpenForSale
      query(aug15, aug01, showInventoryEveryman).status ==> OpenForSale

      // custom tests
      query(aug05, jul10, showInventoryCats).status ==> SaleNotStarted
      query(aug05, jul11, showInventoryCats).status ==> SaleNotStarted
      query(aug05, jul12, showInventoryCats).status ==> OpenForSale
      query(aug05, jul13, showInventoryCats).status ==> OpenForSale
      query(aug05, jul30, showInventoryCats).status ==> OpenForSale
      query(aug05, jul31, showInventoryCats).status ==> OpenForSale
      query(aug05, aug01, showInventoryCats).status ==> SoldOut
      query(aug05, aug02, showInventoryCats).status ==> SoldOut
      query(aug05, aug05, showInventoryCats).status ==> SoldOut
      query(aug05, aug06, showInventoryCats).status ==> InThePast
      queryC(aug30, aug31).status ==> InThePast
    }

  }}
}
