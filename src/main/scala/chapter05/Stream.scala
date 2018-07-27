package chapter05

import chapter05.Stream._

sealed trait Stream[+A]{

  def map1[B](f: A => B):Stream[B] = unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def take1(i: Int):Stream[A] = unfold((i,this)){
    case (n , Cons(h, t)) if n > 0 => Some((h(), (n-1, t())))
    case _ => None
  }

  def takeWhile1(p: A => Boolean):Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def drop(n: Int):Stream[A] = {
    this match {
      case Cons(h, t) if(n > 0) => t().drop(n - 1)
      case _ => this
    }
  }

  def take(n:Int):Stream[A] = {
    this match {
      case Cons(h, t) if(n > 0) => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if(p(h())) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, b) => if(p(h)) cons(h, b) else empty )
  }

  def headOption2: Option[A] = foldRight(None:Option[A])((h, b) => Some(h))

  def map[B](f: A => B):Stream[B] = foldRight(empty[B])((h, b) => cons(f(h), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, b) => if(p(h)) cons(h, b) else b)

  def append[B >: A](b:Stream[B]):Stream[B] = foldRight(b)((h, t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]):Stream[B] = foldRight(empty[B])((h, b) => f(h) append b)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)){
    case (Empty, Empty) => None
    case (Cons(a, ta), Empty) => Some(((Some(a()), None), (ta(), Empty)))
    case (Empty, Cons(b, tb)) => Some(((None, Some(b())), (Empty, tb())))
    case (Cons(a, ta), Cons(b, tb)) => Some(((Some(a()), Some(b())), (ta(), tb())))
  }

  def startsWith[A](prefix:Stream[A]):Boolean = this.zipAll(prefix).takeWhile(_._2.isDefined).forAll(p => p._1 == p._2)

  def tails:Stream[Stream[A]] = unfold(this){
    case Empty => None
    case Cons(h, t) => Some((Cons(h, t), t()))
  } append Stream(Stream())

  def scanRight[B](z: => B)(f: (A, => B) => B):Stream[B] = ???

  //this.tails.map(_.foldRight(z)(f))


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def zipWith1[A,B,C](streamA:Stream[A], streamB:Stream[B], f:(A, B) => C):Stream[C] = unfold((streamA, streamB)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }


  def constant1(i: Int): Stream[Int] = unfold(i)(x => Some((x, x)))

  def ones1 : Stream[Int] = unfold(1)(x => Some((1, 1)))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((x, y)) => Cons(() => x, () => unfold(y)(f))
      case None => Stream.empty
    }
  }


  def fibs1:Stream[Int] = unfold((0,1)) ( s => Some((s._1,(s._2, s._1 + s._2))) )

  def from1(i: Int):Stream[Int] = unfold(i)(s => Some((s, s+1)))

  def fibs:Stream[Int] = add(0, 1)

  private def add(x:Int, y:Int):Stream[Int] = Cons(() => x, () => add(y, x + y))

  def from(i: Int):Stream[Int] = Cons(() => i, () => from(i+1))

  def constant[A](i: A):Stream[A] = Cons(() => i, () => constant(i))

  def zipWith[A,B,C](streamA:Stream[A], streamB:Stream[B], f:(A, B) => C):Stream[C] = unfold((streamA, streamB)) {
    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(a, ta), Cons(b, tb)) => Some((f(a(), b()), (ta(), tb())))
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
