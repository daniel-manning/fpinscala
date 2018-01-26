package chapter03

object Tail {
  def setHead[A](i: A, value: List[A]): List[A] = {
    value match {
      case Nil => Nil
      case Cons(_, xs) => Cons(i, xs)
    }
  }


  def tail[A](as:List[A]):List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n==0) {
      l
    }else {
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(a, as) => if(f(a)) {
            dropWhile(as, f)
          }else {
            l
          }
      }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, Nil) => Nil
      case Cons(a, as) => Cons(a,init(as))
    }
  }

}
