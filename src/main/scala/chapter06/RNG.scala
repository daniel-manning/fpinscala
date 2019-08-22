package chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble/Int.MaxValue.toDouble)

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

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doubleValue, rngNext) = double(rng)
    val (intValue, rngFinal) = nonNegativeInt(rngNext)
    ((doubleValue, intValue), rngFinal)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldRight((List[Int](), rng)) {
      (_, b) => { val (intVal, rngNext) = b._2.nextInt; ( b._1 :+ intVal, rngNext)}
    }
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

