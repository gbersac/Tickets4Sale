package ticket4Sale

import utest._
import upickle.default._

object ShowsJSONFormatterTests extends TestSuite {
  val showInventory = ShowInventory(List(
    Show("Cats", Date("2018-06-01").get, Musical),
    Show("Comedy of Errors", Date("2018-07-01").get, Comedy),
    Show("Everyman", Date("2018-08-01").get, Drama),
  ))

  val jan01 = Date.apply("2018-01-01").get
  val jul01 = Date.apply("2018-07-01").get
  val aug01 = Date.apply("2018-08-01").get
  val aug15 = Date.apply("2018-08-15").get

  val tests = Tests {

    test("Json formatting - example 1") {
      val result = """{"inventory": [
        {"genre": "musical", "shows":[
        {"title": "cats", "tickets_left": "200", "tickets_available": "0",
        "status": "sale not started"}
        ]},
        {"genre": "comedy", "shows":[
        {"title": "comedy of errors", "tickets_left": "200", "tickets_available":
        "0", "status": "sale not started"}
        ]}
        ]}"""
      ShowsJSONFormatter(showInventory.query(jul01, jan01)) ==> read[ujson.Obj](result)
    }

    test("Json formatting - example 2") {
      val result = """{"inventory": [
      {"genre": "musical", "shows":[
      {"title": "cats", "tickets_left": "50", "tickets_available": "5",
      "status": "open for sale"}
      ]},
      {"genre": "comedy", "shows":[
      {"title": "comedy of errors", "tickets_left": "100", "tickets_available":
      "10", "status": "open for sale"}
      ]},
      {"genre": "drama", "shows":[
      {"title": "everyman", "tickets_left": "100", "tickets_available": "10",
      "status": "open for sale"}
      ]}
      ]}"""
      ShowsJSONFormatter(showInventory.query(aug15, aug01)) ==> read[ujson.Obj](result)
    }

  }

}
