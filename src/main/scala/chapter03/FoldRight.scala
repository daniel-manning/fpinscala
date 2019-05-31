package chapter03

object FoldRight {

  def reverse[A](as: List[A]):List[A] = foldLeft(as, List[A]())((a:List[A], b:A) => Cons(b,a))

  def length[A](as: List[A]):Int = foldRight(as,0)((_,count) => count+1)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]):Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]):Double =
    foldRight(ns, 1.0)(_ * _)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(acc:B, la:List[A]): B ={
        la match {
          case Nil => acc
          case Cons(a, as) => go(f(acc,a), as)
        }
    }

    go(z, as)
  }

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
        case Nil => z
        case Cons(l, ls) => foldLeft2(ls, f(z, l))(f)
      }
    }

  /*def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
     foldRight(as,identity())((b,g,x) => g(f(x,b)))
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft()()
  }*/

  def append[A](xs:List[A], as:List[A]):List[A] = {
    foldRight(xs, as)((a:A,b:List[A]) => Cons(a,b))
  }

  def flatten[A](xs:List[List[A]]):List[A] = {
    foldRight(xs, List[A]())(append)
  }

  def addOneToAll(xs:List[Int]):List[Int] = {
    foldRight(xs, List[Int]())((a:Int,b:List[Int]) => Cons(a+1,b))
  }

  def convertElementsToString(xs:List[Double]):List[String] = {
    foldRight(xs, List[String]())((a:Double,b:List[String]) => Cons(a.toString,b))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((a:A,b:List[B]) => Cons(f(a),b))
  }

  def filter[A](as: List[A])(p: A => Boolean): List[A] = {
    foldRight(as, List[A]())((a:A,b:List[A]) => if(p(a)) Cons(a,b) else b)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  def filter2[A](as: List[A])(p: A => Boolean): List[A] = {
    flatMap(as)((a:A) => if(p(a)){ List(a) } else List())
  }

  def plusTwoLists(as: List[Int], bs: List[Int]):List[Int] = {
      val a:(List[Int],List[Int]) = foldRight(as, (List[Int](),reverse(bs)))((a:Int,b:(List[Int],List[Int])) =>
        (Cons(a + (b._2 match {case Nil => 0; case Cons(c,_) => c }),b._1), b._2 match {
          case Nil => Nil;
          case Cons(_, d) => d
        }))
   a._1
  }

  //This is the answer
  def addTwoListsPairwise(as: List[Int], bs: List[Int]):List[Int] = {
    (as,bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a,at), Cons(b,bt)) => Cons(a+b, addTwoListsPairwise(at, bt))
    }
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f:(A,B) => C):List[C] = {
    (as,bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a,at), Cons(b,bt)) => Cons(f(a,b), zipWith(at, bt)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup,sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(a,at), Cons(b,bt)) => if(a == b) hasSubsequence(at,bt) else hasSubsequence(at,sub)
    }
  }
}
