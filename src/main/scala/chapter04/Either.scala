package chapter04

import chapter03.{Cons, List}

import scala.util.{Failure, Success, Try}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing]{
  override def map[B](f: Nothing => B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}


case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(c => f(value,c))
}

object EitherFunctions {

  def withTry[A](t: Try[A]):Either[String, A] = {
    t match {
      case _:Failure[A] => Left("Try has failed!")
      case Success(x) => Right(x)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(List()))((a:Either[E,A], b:Either[E, List[A]]) => b.flatMap(c => a.map(sa => Cons(sa, c))))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(List()))((a:A, b:Either[E, List[B]]) => b.flatMap(c => f(a).map(sa => Cons(sa, c))))
  }

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
}