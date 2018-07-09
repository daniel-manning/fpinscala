package chapter05

import org.scalatest.{FlatSpec, Matchers}

class StreamSpecs extends FlatSpec with Matchers {

  "toList" should "convert stream to List" in {
    Stream(1,2,3).toList shouldBe List(1,2,3)
  }

  "drop" should "remove the first n items from a stream" in {
    Stream(1,2,3,4,5).drop(3).toList shouldBe List(4,5)
  }

  "take" should "keep the first n items from a stream" in {
    Stream(1,2,3,4,5).take(3).toList shouldBe List(1,2,3)
  }

  "takeWhile" should "keep the first items Satisfying a predicate" in {
    Stream(2,4,6,8,9,11).takeWhile(_ % 2 == 0).toList shouldBe List(2,4,6,8)
  }

  "exists" should "find a value in stream" in {
    Stream(2,4,6,8,9,11).exists(_ % 2 == 0) shouldBe true
  }

  "exists" should "not find a matching value in stream" in {
    Stream(1,3,5,7,9).exists(_ % 2 == 0) shouldBe false
  }

  "forAll" should "be correct for a stream of matching values" in {
    Stream(2,4,6,8).forAll(_ % 2 == 0) shouldBe true
  }

  "forAll" should "be incorrect for a stream with a non-matching value" in {
    Stream(2,4,5,8).forAll(_ % 2 == 0) shouldBe false
  }

  "takeWhile2" should "keep the first items Satisfying a predicate" in {
    Stream(2,4,6,8,9,11).takeWhile2(_ % 2 == 0).toList shouldBe List(2,4,6,8)
  }

  "headOption2" should "give Nothing for an empty stream" in {
    Stream().headOption2 shouldBe None
  }

  "headOption2" should "give a value for a not empty stream" in {
    Stream(1,2,3).headOption2 shouldBe Some(1)
  }

  "map" should "map over stream" in {
    Stream(1,2,3).map(_ + 1).toList shouldBe List(2,3,4)
  }

  "map" should "map over empty stream" in {
    Empty.map((n:Int) => n + 1) shouldBe Empty
  }

  "filter" should "remove elements of stream that do not match a predicate" in {
    Stream(1,2,3,4).filter(_ % 2 == 0).toList shouldBe List(2,4)
  }

  "append" should "add a stream at the end of a previous stream" in {
    Stream(1,2,3,4).append(Stream(5,6)).toList shouldBe List(1,2,3,4,5,6)
  }

  "flatMap" should "take a function making lists and flatten them" in {
    Stream(1, 2, 3).flatMap(i => Stream(i,i)).toList shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "constant" should "return an infinite stream of a given value" in {
    Stream.constant(5).take(5).toList shouldBe List(5,5,5,5,5)
  }

  "from" should "return an infinite stream of incrementing integers" in {
    Stream.from(0).take(5).toList shouldBe List(0,1,2,3,4)
  }

  "fibonnaci" should "return an infinite Stream of fibonacci numbers" in {
    Stream.fibs.take(7).toList shouldBe List(0,1,1,2,3,5,8)
  }

  "unfold" should "return an infinite Stream of fibonacci numbers given a fibonacci function" in {
    Stream.unfold(0)(x => Some((x, x+1))).take(7).toList shouldBe List(0,1,2,3,4,5,6)
  }

  "unfold" should "return an infinite Stream of fibonacci numbers" in {
    Stream.fibs1.take(7).toList shouldBe List(0,1,1,2,3,5,8)
  }

  "unfold" should "return an infinite stream of incrementing integers" in {
    Stream.from1(0).take(5).toList shouldBe List(0,1,2,3,4)
  }

  "unfold" should "return an infinite stream of a given value" in {
    Stream.constant1(5).take(5).toList shouldBe List(5,5,5,5,5)
  }

  "unfold" should "return an infinite stream of ones" in {
    Stream.ones1.take(5).toList shouldBe List(1,1,1,1,1)
  }

  "map1" should "return a transformed stream" in {
    val numberStream = Stream(1,2,3)
    numberStream.map1(x => x + 1).toList shouldBe List(2,3,4)
  }

  "take1" should "return first n elements of a stream" in {
    val numberStream = Stream(1,2,3,4,5,6,7,8)
    numberStream.take1(4).toList shouldBe List(1,2,3,4)
  }

  "takeWhile1" should "return first elements of a stream which match the predicate" in {
    val numberStream = Stream(1,2,3,4,5,6,7,8)
    def predicate: Int => Boolean = (x:Int) => x < 5
    numberStream.takeWhile1(predicate).toList shouldBe List(1,2,3,4)
  }

  "zipWith" should "return a stream zipped from two others" in {
    val firstStream = Stream(1,2,3,4,5,6)
    val secondStream = Stream(7,8,9,10,11,12)

    Stream.zipWith(firstStream, secondStream, (x:Int, y:Int) => x * y).toList shouldBe List(7, 16, 27, 40, 55, 72)
  }

  "zipAll" should "return a stream zipped from two others" in {
    val firstStream = Stream(1,2,3,4,5,6)
    val secondStream = Stream(7,8,9,10)

    firstStream.zipAll(secondStream).toList shouldBe List((Some(1), Some(7)), (Some(2), Some(8)), (Some(3), Some(9)),
      (Some(4), Some(10)), (Some(5), None), (Some(6), None))
  }
}
