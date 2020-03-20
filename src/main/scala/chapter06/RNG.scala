
package chapter06

trait RNG {
  def nextInt: (RNG, Int)
}

object RNG {

  def unit[A](a: A): State[RNG, A] =
    State(rng => (rng, a))

  def map[A,B](s: State[RNG, A])(f: A => B): State[RNG, B] =
    State(rng => {
      val (rng2, a) = s.run(rng)
      (rng2, f(a))
    })

  def mapDash[A,B](s: State[RNG, A])(f: A => B): State[RNG, B] =
    flatMap(s)(a => unit(f(a)))

  def flatMap[A,B](f: State[RNG, A])(g: A => State[RNG, B]): State[RNG, B] = {
    State(rng => {
      val (rng2, a) = f.run(rng)
      g(a).run(rng2)
    })
  }

  def nonNegativeEven: State[RNG, Int] =
    map(State(rng => nonNegativeInt(rng)))(i => i - i % 2)

  def double2: State[RNG, Double] =
    map(State(rng => nonNegativeInt(rng)))(i => i.toDouble/Int.MaxValue.toDouble)

  def map2[A,B,C](ra: State[RNG, A], rb: State[RNG, B])(f: (A, B) => C): State[RNG, C] =
    State {
      (rng: RNG) =>  {
        val (rng2, a) = ra.run(rng)
        val (rng3, b) = rb.run(rng2)

        (rng3, f(a, b))
      }
    }

  def both[A,B](ra: State[RNG, A], rb: State[RNG, B]): State[RNG, (A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: State[RNG, (Int, Double)] =
      both(State(_.nextInt), State(double))

  val randDoubleInt: State[RNG, (Double, Int)] =
    both(State(double), State(_.nextInt))

  def sequence[A](fs: List[State[RNG, A]]): State[RNG, List[A]] =
    State( rng => {
      fs.foldRight((rng, List.empty[A])){
        (a, b) => {
          val (nrng, na) = a.run(b._1)
          (nrng, na :: b._2)
        }
      }
    })

  def nonNegativeLessThan(n: Int): State[RNG, Int] =
   flatMap(State(rng => nonNegativeInt(rng))){
     (i:Int) => {
       val mod = i % n
       if (i + (n-1) - mod >= 0)
         unit(mod)
       else nonNegativeLessThan(n)
     }
   }

  @scala.annotation.tailrec
  def nonNegativeInt(rng: RNG): (RNG, Int) = {
    val (rngNext, intValue) = rng.nextInt
    if(intValue >= 0) (rngNext, intValue)
    else nonNegativeInt(rngNext)
  }

  @scala.annotation.tailrec
  def double(rng: RNG): (RNG, Double) = {
    val (rngNext, intValue) = nonNegativeInt(rng)
    if(intValue != Int.MaxValue){
      (rngNext, (intValue.toDouble/Int.MaxValue.toDouble))
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

  def ints(count: Int)(rng: RNG): (RNG, List[Int]) =
    (1 to count).foldRight((rng, List[Int]())) {
        (_, b) => {
          val (rngNext, intVal) = b._1.nextInt
          (rngNext, b._2 :+ intVal)
        }
    }

}

case class SimpleRNG(seed:Long) extends RNG
{
  def nextInt: (RNG, Int) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (nextRNG, n)
  }
}
