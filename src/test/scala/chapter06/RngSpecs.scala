package chapter06

import org.scalatest.{FlatSpec, Matchers}


class RngSpecs extends FlatSpec with Matchers {

  "rng nonnegative number" should "return a value between 0 and Int.Max" in {
    val rng = SimpleRNG(0l)
    val (randomValue, _) = RNG.nonNegativeInt(rng)
    randomValue >= 0 shouldBe true
    randomValue <= Int.MaxValue shouldBe true
  }

  "rng double" should "return a double value between 0 and 1" in {
    val rng = SimpleRNG(0l)
    val (randomValue:Double, _) = RNG.double(rng)
    randomValue >= 0 shouldBe true
    randomValue < 1 shouldBe true
  }

  "rng intDouble" should "return an (int, double) tuple" in {
    val rng = SimpleRNG(0l)
    val ((intValue, doubleValue), _) = RNG.intDouble(rng)
    intValue >= 0 shouldBe true
    intValue <= Int.MaxValue shouldBe true
    doubleValue >= 0 shouldBe true
    doubleValue < 1 shouldBe true
  }
}
