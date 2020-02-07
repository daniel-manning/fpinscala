package chapter07

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.TimeUnit

/*
object Par {


  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = (a:A) => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A])){
    (a, b) => map2(a, b)(_ :: _)
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    sequence(as.foldRight(List.empty[Par[Option[A]]]){
      (a, b) => asyncF(if(f(a)) Some(a) else None) :: b
    })

  def parallelOp[A, B](ints: IndexedSeq[A])(z: A)(f: (A, A) => A ): Par[A] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse z)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(parallelOp(l)(z)(f)), Par.fork(parallelOp(r)(z)(f)))(f)
    }

  def maxInt(ints: IndexedSeq[Int]) = parallelOp(ints)(0)(_.max(_))

}

case class MapFuture[A, B, C](a: Future[A], b:Future[B]) extends  Future[C] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

  override def isCancelled: Boolean = ???

  override def isDone: Boolean = ???

  override def get(): C = get(Long.MaxValue, TimeUnit.MILLISECONDS)

  override def get(timeout: Long, unit: TimeUnit): C = ???
}*/
