package chapter06

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class RngSpecs extends FlatSpec with Matchers with PropertyChecks {

  val longs: Gen[Long] = Gen.chooseNum(Long.MinValue, Long.MaxValue)

  "rng nonnegative number" should "return a value between 0 and Int.Max" in {
    forAll(longs){ seed =>
      val rng = SimpleRNG(seed)
      val (_, randomValue) = RNG.nonNegativeInt(rng)
      randomValue >= 0 shouldBe true
      randomValue <= Int.MaxValue shouldBe true
    }
  }

  "rng double" should "return a double value between 0 and 1" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val (_, randomValue: Double) = RNG.double(rng)
      randomValue >= 0 shouldBe true
      randomValue < 1 shouldBe true
    }
  }

  "rng intDouble" should "return an (int, double) tuple" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val (_, (intValue, doubleValue)) = RNG.intDouble(rng)
      intValue >= 0 shouldBe true
      intValue <= Int.MaxValue shouldBe true
      doubleValue >= 0 shouldBe true
      doubleValue < 1 shouldBe true
    }
  }

  "rng doubleInt" should "return a (double, int) tuple" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val (_, (doubleValue, intValue)) = RNG.doubleInt(rng)
      intValue >= 0 shouldBe true
      intValue <= Int.MaxValue shouldBe true
      doubleValue >= 0 shouldBe true
      doubleValue < 1 shouldBe true
    }
  }

  "rng double3" should "return a (double, double, double) 3-tuple" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val (_, (double1, double2, double3)) = RNG.double3(rng)
      double1 >= 0 shouldBe true
      double1 < 1 shouldBe true
      double2 >= 0 shouldBe true
      double2 < 1 shouldBe true
      double3 >= 0 shouldBe true
      double3 < 1 shouldBe true
    }
  }

  "rng ints" should "return a list of ints" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val intList = RNG.ints(5)(rng)._2
      intList.length shouldBe 5
    }
  }

  "rng double2" should "return a double value between 0 and 1" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val (_, randomValue: Double) = RNG.double2.run(rng)
      randomValue >= 0 shouldBe true
      randomValue < 1 shouldBe true
    }
  }


  "sequence" should "return a random list from a list of randoms" ignore {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)

      val listOfRandoms = List(RNG.randDoubleInt, RNG.randDoubleInt, RNG.randDoubleInt)

      val randomList = RNG.sequence(listOfRandoms)

      println(randomList.run(rng))
    }
  }

}
