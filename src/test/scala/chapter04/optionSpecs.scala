package chapter04

import org.scalatest.{FlatSpec, Matchers}

class optionSpecs extends FlatSpec with Matchers {

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

}
