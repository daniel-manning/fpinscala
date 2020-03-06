
package chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

 def unit[A](a: A): State[RNG, A] =
    State(rng => (a, rng))

  def map[A,B](s: State[RNG, A])(f: A => B): State[RNG, B] =
    State(rng => {
      val (a, rng2) = s.run(rng)
      (f(a), rng2)
    })

  def mapDash[A,B](s: State[RNG, A])(f: A => B): State[RNG, B] =
    flatMap(s)(a => unit(f(a)))

  def flatMap[A,B](f: State[RNG, A])(g: A => State[RNG, B]): State[RNG, B] = {
    State(rng => {
      val (a, rng2) = f.run(rng)
      g(a).run(rng2)
    })
  }

  /*def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){
      (i:Int) => val mod = i % n
        if (i + (n-1) - mod >= 0)
          unit(mod)
        else nonNegativeLessThan(n)
    }

  def nonNegativeEven: State[RNG, Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: State[RNG, Double] =
    map(nonNegativeInt)(i => i.toDouble/Int.MaxValue.toDouble)*/

  def map2[A,B,C](ra: State[RNG, A], rb: State[RNG, B])(f: (A, B) => C): State[RNG, C] =
    State {
      (rng: RNG) =>  {
        val (a, rng2) = ra.run(rng)
        val (b, rng3) = rb.run(rng2)

        (f(a, b), rng3)
      }
    }

  def both[A,B](ra: State[RNG, A], rb: State[RNG, B]): State[RNG, (A,B)] =
    map2(ra, rb)((_, _))

/*  val randIntDouble: State[RNG, (Int, Double)] =
      both((rng: RNG) => (rng.nextInt), double)

  val randDoubleInt: State[RNG, (Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[State[RNG, A]]): State[RNG, List[A]] = {
    rng => {
      fs.foldRight((List.empty[A], rng)){
        (a, b) => {
          val (na, nrng) = a(b._2)

          (na :: b._1, nrng)
        }
      }
    }
  }*/

  @scala.annotation.tailrec
  def nonNegativeInt(rng: RNG): (RNG, Int) = {
    val (intValue, rngNext) = rng.nextInt
    if(intValue >= 0) (rngNext, intValue)
    else nonNegativeInt(rngNext)
  }

  @scala.annotation.tailrec
  def double(rng: RNG): (RNG, Double) = {
    val (rngNext, intValue) = nonNegativeInt(rng)
    if(intValue != Int.MaxValue){
      (rngNext, intValue.toDouble/Int.MaxValue.toDouble)
    }else{
      double(rngNext)
    }
  }

  def intDouble(rng: RNG): (RNG, (Int,Double)) = {
    val (rngNext, intValue) = nonNegativeInt(rng)
    val (rngFinal, doubleValue) = double(rngNext)
    (rngFinal, (intValue, doubleValue))
  }

  def doubleInt(rng: RNG): (RNG, (Double,Int)) = {
    val (rngNext, doubleValue) = double(rng)
    val (rngFinal, intValue) = nonNegativeInt(rngNext)
    (rngFinal, (doubleValue, intValue))
  }

  def double3(rng: RNG): (RNG, (Double,Double,Double)) = {
    val (rng1, double1) = double(rng)
    val (rng2, double2) = double(rng1)
    val (rng3, double3) = double(rng2)
    (rng3, (double1, double2, double3))
  }

  /*def ints(count: Int)(rng: RNG): (RNG, List[Int]) = {
    (1 to count).foldRight((List[Int](), rng)) {
      (_, b) => { val (intVal, rngNext) = b._2.nextInt; ( b._1 :+ intVal, rngNext)}
    }
  }*/
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
