package chapter05

import chapter05.Stream._

sealed trait Stream[+A]{

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

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

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


  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
