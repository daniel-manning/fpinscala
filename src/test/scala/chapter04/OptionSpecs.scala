package chapter04

import org.scalatest.{FlatSpec, Matchers}
import chapter03.{Cons, List}

import scala.util.Try


class OptionSpecs extends FlatSpec with Matchers {

  def sqrt(n:Int) = if(n<0) None else Some(Math.sqrt(n))
  def isEven(n:Int) = n % 2 == 0

  "map" should "map over some" in {
    val some = Some(1)
    some.map(_ + 1) shouldBe Some(2)
  }

  "map" should "map over None" in {
    val none = None
    none.map((n:Int) => n + 1) shouldBe None
  }

  "flatMap" should "map over some" in {
    val some = Some(1)

    some.flatMap(sqrt) shouldBe Some(1.0)
  }

  "flatMap" should "map over some for bad values" in {
    val someBadValue = Some(-1)
    someBadValue.flatMap(sqrt) shouldBe None
  }

  "flatMap" should "map over None" in {
    val none = None
    none.flatMap(sqrt) shouldBe None
  }

  "getOrElse" should "map over some" in {
    val some = Some(2)
    some.getOrElse(1) shouldBe 2
  }

  "getOrElse" should "map over None" in {
    val none = None
    none.getOrElse(1) shouldBe 1
  }

  "orElse" should "map over some" in {
    val some = Some(2)
    some.orElse(Some(1)) shouldBe Some(2)
  }

  "orElse" should "map over None" in {
    val none = None
    none.orElse(Some(1)) shouldBe Some(1)
  }

  "filter" should "map over some" in {
    val some = Some(2)
    some.filter(isEven) shouldBe Some(2)
  }

  "filter" should "map over some bad value" in {
    val some = Some(1)
    some.filter(isEven) shouldBe None
  }

  "filter" should "map over None" in {
    val none = None
    none.filter(isEven) shouldBe None
  }

  //rewrite
  "flatMap2" should "map over some" in {
    val some = Some(1)

    some.flatMap2(sqrt) shouldBe Some(1.0)
  }

  "flatMap2" should "map over some for bad values" in {
    val someBadValue = Some(-1)
    someBadValue.flatMap2(sqrt) shouldBe None
  }

  "flatMap2" should "map over None" in {
    val none = None
    none.flatMap2(sqrt) shouldBe None
  }

  "orElse2" should "map over some" in {
    val some = Some(2)
    some.orElse2(Some(1)) shouldBe Some(2)
  }

  "orElse2" should "map over None" in {
    val none = None
    none.orElse2(Some(1)) shouldBe Some(1)
  }

  "filter2" should "map over some" in {
    val some = Some(2)
    some.filter2(isEven) shouldBe Some(2)
  }

  "filter2" should "map over some bad value" in {
    val some = Some(1)
    some.filter2(isEven) shouldBe None
  }

  "filter2" should "map over None" in {
    val none = None
    none.filter2(isEven) shouldBe None
  }

  "varience" should "give the correct result for list of numbers" in {
    val someList = Seq(1.1,2.2,3.3,4.4)
    assert(OptionFunctions.areEqualDouble(OptionFunctions.variance(someList).getOrElse(0.0), 1.5125, 4))
  }

  "varience" should "give the correct result for empty list" in {
    val none = Seq.empty
    OptionFunctions.variance(none) shouldBe None
  }

  "map2" should "give the correct result for supplied params" in {
    OptionFunctions.map2(Some(4), Some(2))((a:Int,b:Int) => a + b) shouldBe Some(6)
  }

  it should "give the correct result for None first param" in {
    OptionFunctions.map2(None, Some(2))((a:Int,b:Int) => a + b) shouldBe None
  }

  it should "give the correct result for None second param" in {
    OptionFunctions.map2(Some(2), None)((a:Int,b:Int) => a + b) shouldBe None
  }

  it should "give the correct result for None params" in {
    OptionFunctions.map2(None, None)((a:Int,b:Int) => a + b) shouldBe None
  }

  "sequence" should "give the correct result for a list of all some" in {
    val testData = List(Some(1), Some(2), Some(3))
    val expectedResult: Option[List[Int]] = Some(List(1, 2, 3))
    OptionFunctions.sequence(testData) shouldBe expectedResult
  }

  it should "give the correct result of None for list containing None" in {
    val testData = List(Some(1), None, Some(3))
    val expectedResult: Option[List[Int]] = None
    OptionFunctions.sequence(testData) shouldBe expectedResult
  }


  "traverse" should "give the correct result for a list of all correct" in {
    val testData:List[String] = List("1", "2", "3")
    val expectedResult: Option[List[Int]] = Some(List(1, 2, 3))
    OptionFunctions.traverse[String, Int](testData)((s:String) => OptionFunctions.withTry(Try(s.toInt))) shouldBe expectedResult
  }

  it should "give the correct result of None for list containing None" in {
    val testData:List[String] = List("1", "&", "3")
    val expectedResult: Option[List[Int]] = None
    OptionFunctions.traverse[String, Int](testData)((s:String) => OptionFunctions.withTry(Try(s.toInt))) shouldBe expectedResult
  }

  "sequence2" should "give the correct result for a list of all some" in {
    val testData = List(Some(1), Some(2), Some(3))
    val expectedResult: Option[List[Int]] = Some(List(1, 2, 3))
    OptionFunctions.sequence2(testData) shouldBe expectedResult
  }

  it should "give the correct result of None for list containing None" in {
    val testData = List(Some(1), None, Some(3))
    val expectedResult: Option[List[Int]] = None
    OptionFunctions.sequence2(testData) shouldBe expectedResult
  }

}
