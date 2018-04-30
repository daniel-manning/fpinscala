package chapter04

import chapter03.{Cons, List}

import scala.util.{Failure, Success, Try}

//sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if(f(a)) Some(a) else None
    case None => None
  }


  //rewrite in terms of map and getOrElse
  def flatMap2[B](f: A => Option[B]): Option[B] = this map f getOrElse None

  def orElse2[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter2(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
}

object OptionFunctions{

  def withTry[A](t: Try[A]):Option[A] = {
   t match {
     case _:Failure[A] => None
     case Success(x) => Some(x)
   }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).map(m => xs.map(y => math.pow(y - m, 2)).sum / xs.length)
  def areEqualDouble(a:Double, b:Double, precision:Int): Boolean = Math.abs(a - b) <= Math.pow(10, -precision)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(sa => b.map(sb => f(sa, sb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List()))((a:Option[A], b:Option[List[A]]) => b.flatMap(sb => a.map(sa => Cons(sa, sb))))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(List()))((a:A, b:Option[List[B]]) => b.flatMap(sb => f(a).map(sa => Cons(sa, sb))))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }
}
