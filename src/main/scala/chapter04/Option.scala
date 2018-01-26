package chapter04

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