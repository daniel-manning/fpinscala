package chapter03

import org.scalatest.{FlatSpec, Matchers}

class TailSpec extends FlatSpec with Matchers {

  "Tail with a list" should "return the tail of a list" in {
    Tail.tail(List(1,2,3,4)) shouldBe List(2,3,4)
  }

  "Tail with a Nil" should "return Nil" in {
    Tail.tail(Nil) shouldBe Nil
  }

  "Tail with one member" should "return Nil" in {
    Tail.tail(List(1)) shouldBe Nil
  }

  "setHead with a list" should "return a list with a new head" in {
    Tail.setHead(4, List(1,2,3,4)) shouldBe List(4,2,3,4)
  }

  "drop zero with a list" should "return the list" in {
    Tail.drop(List(1,2,3,4), 0) shouldBe List(1,2,3,4)
  }

  "drop 2 with a list" should "return the last two elements of the list" in {
    Tail.drop(List(1,2,3,4), 2) shouldBe List(3,4)
  }


  "dropWhile with a list and a pred of smaller than 3" should "return the last element of the list" in {
    Tail.dropWhile(List(1,2,3,4), (x:Int)=> x < 3) shouldBe List(3,4)
  }

  "dropWhile with a list and a pred of smaller than 1" should "return the list" in {
    Tail.dropWhile(List(1,2,3,4), (x:Int)=> x < 1) shouldBe List(1,2,3,4)
  }

  "init with a list" should "return the list without the last element" in {
    Tail.init(List(1,2,3,4)) shouldBe List(1,2,3)
  }


}
