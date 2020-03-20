package chapter08

import cats.data.State
import chapter06.{RNG, SimpleRNG}

object PropertyTesting{
  val falseProp = new Prop{
    def check = false
  }

  val trueProp = new Prop {
    def check = true
  }


  val result = (trueProp && falseProp).check

  println(s"result is $result")
}

trait Prop {
  def check: Boolean
  def &&(that:Prop): Prop = new Prop {
    def check = Prop.this.check && that.check
  }
}


case class Gen[A](sample: State[RNG,A]) {
  def unit[A](a: => A): Gen[A] = Gen(sample.map(_ => a))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt)
      .map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] = Gen(State{rng =>
    val (rngNext, int) = rng.nextInt
    if(int < 0) (rngNext, false)
    else (rngNext, true)
  })


  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(
    State(r =>  (1 to n).foldRight((r, List[A]())){
        (_, b) => {
          val (rngNext, a) = g.sample.run(b._1).value

          (rngNext, a :: b._2)
        }
      }
    ))

  State
}

object ohNo extends App {
  val a = Gen.choose(0, 12)
    .sample.run(SimpleRNG(48l))
  println(a.value._2)

  val b = Gen.boolean.sample.run(SimpleRNG(42l))
  println(b.value._2)

  val c = Gen.listOfN(7, Gen.choose(0, 12)).sample.run(SimpleRNG(0l))
  println(c.value._2)
}

