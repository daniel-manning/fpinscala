package chapter04

import chapter03.List
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class EitherSpecs extends FlatSpec with Matchers {

  def sqrt(n:Int) = if(n<0) Left("Complex number!") else Right(Math.sqrt(n))

  "map" should "map over Right" in {
    val right = Right(1)
    right.map(_ + 1) shouldBe Right(2)
  }

  "map" should "map over Left" in {
    val none = Left("Complex number!")
    none.map((n:Int) => n + 1) shouldBe Left("Complex number!")
  }

  "flatMap" should "map over Right" in {
    val right = Right(1)

    right.flatMap(sqrt) shouldBe Right(1.0)
  }

  "flatMap" should "map over some for bad values" in {
    val someBadValue = Right(-1)
    someBadValue.flatMap(sqrt) shouldBe Left("Complex number!")
  }

  "flatMap" should "map over Left" in {
    val left = Left("Some old problem")
    left.flatMap(sqrt) shouldBe Left("Some old problem")
  }

  "map2" should "give the correct result for supplied params" in {
    Right(4).map2(Right(2))((a:Int,b:Int) => a + b) shouldBe Right(6)
  }

  it should "give the correct result for Left first param" in {
    Left("").map2(Right(2))((a:Int,b:Int) => a + b) shouldBe Left("")
  }

  it should "give the correct result for Left second param" in {
    Right(2).map2(Left(""))((a:Int,b:Int) => a + b) shouldBe Left("")
  }

  it should "give the correct result for Left params" in {
    Left("").map2(Left(""))((a:Int,b:Int) => a + b) shouldBe Left("")
  }

  "sequence" should "give the correct result for a list of all data" in {
    val testData: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    val expectedResult: Either[String, List[Int]] = Right(List(1, 2, 3))
    EitherFunctions.sequence(testData) shouldBe expectedResult
  }

  it should "give the correct result of None for list containing Left" in {
    val testData: List[Either[String, Int]] = List(Right(1), Left(""), Right(3))
    val expectedResult: Either[String, List[Int]] = Left("")
    EitherFunctions.sequence(testData) shouldBe expectedResult
  }

  "traverse" should "give the correct result for a list of all correct" in {
    val testData:List[String] = List("1", "2", "3")
    val expectedResult: Either[String, List[Int]] = Right(List(1, 2, 3))
    EitherFunctions.traverse(testData)((s:String) => EitherFunctions.withTry(Try(s.toInt))) shouldBe expectedResult
  }

  it should "give the correct result of None for list containing Left" in {
    val testData:List[String] = List("1", "&", "3")
    val expectedResult: Either[String, List[Int]] = Left("Try has failed!")
    EitherFunctions.traverse(testData)((s:String) => EitherFunctions.withTry(Try(s.toInt))) shouldBe expectedResult
  }

  "sequence2" should "give the correct result for a list of all data" in {
    val testData: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    val expectedResult: Either[String, List[Int]] = Right(List(1, 2, 3))
    EitherFunctions.sequence2(testData) shouldBe expectedResult
  }

  it should "give the correct result of None for list containing Left" in {
    val testData: List[Either[String, Int]] = List(Right(1), Left(""), Right(3))
    val expectedResult: Either[String, List[Int]] = Left("")
    EitherFunctions.sequence2(testData) shouldBe expectedResult
  }
}
