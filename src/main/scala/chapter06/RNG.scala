package chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (intValue, rngNext) = rng.nextInt
    if(intValue >= 0) (intValue, rngNext)
    else nonNegativeInt(rngNext)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (intValue, rngNext) = nonNegativeInt(rng)
    if(intValue != Int.MaxValue){
      (intValue.toDouble/Int.MaxValue.toDouble, rngNext)
    }else{
      double(rngNext)
    }
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intValue, rngNext) = nonNegativeInt(rng)
    val (doubleValue, rngFinal) = double(rngNext)
    ((intValue, doubleValue), rngFinal)
  }
}

case class SimpleRNG(seed:Long) extends RNG
{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

