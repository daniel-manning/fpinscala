package chapter06

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class RngSpecs extends FlatSpec with Matchers with PropertyChecks {

  val longs: Gen[Long] = Gen.chooseNum(Long.MinValue, Long.MaxValue)

  "rng nonnegative number" should "return a value between 0 and Int.Max" in {
    forAll(longs){ seed =>
      val rng = SimpleRNG(seed)
      val (randomValue, _) = RNG.nonNegativeInt(rng)
      randomValue >= 0 shouldBe true
      randomValue <= Int.MaxValue shouldBe true
    }
  }

  "rng double" should "return a double value between 0 and 1" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val (randomValue: Double, _) = RNG.double(rng)
      randomValue >= 0 shouldBe true
      randomValue < 1 shouldBe true
    }
  }

  "rng intDouble" should "return an (int, double) tuple" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val ((intValue, doubleValue), _) = RNG.intDouble(rng)
      intValue >= 0 shouldBe true
      intValue <= Int.MaxValue shouldBe true
      doubleValue >= 0 shouldBe true
      doubleValue < 1 shouldBe true
    }
  }

  "rng doubleInt" should "return a (double, int) tuple" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val ((doubleValue, intValue), _) = RNG.doubleInt(rng)
      intValue >= 0 shouldBe true
      intValue <= Int.MaxValue shouldBe true
      doubleValue >= 0 shouldBe true
      doubleValue < 1 shouldBe true
    }
  }

  "rng double3" should "return a (double, double, double) 3-tuple" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val ((double1, double2, double3), _) = RNG.double3(rng)
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
      val intList = RNG.ints(5)(rng)._1
      intList.length shouldBe 5
    }
  }

  "rng double2" should "return a double value between 0 and 1" in {
    forAll(longs) { seed =>
      val rng = SimpleRNG(seed)
      val (randomValue: Double, _) = RNG.double2(rng)
      randomValue >= 0 shouldBe true
      randomValue < 1 shouldBe true
    }
  }

}
