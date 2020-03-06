package chapter08
/*
import chapter06.{RNG, State}*/
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
  def unit[A](a: => A): Gen[A] = ???
  def boolean: Gen[Boolean] = ???
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt)
      .map(n => start + n % (stopExclusive - start)))
}

object ohNo extends App {
  val a = Gen.choose(0, 12)
    .sample.run(SimpleRNG(48l))
  println(a.value._2)
}

