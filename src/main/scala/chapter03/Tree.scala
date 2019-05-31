package chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeMethods {
    def size[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
      }
    }

    def maximum(tree:Tree[Int]):Int = {
      tree match {
        case Leaf(a) => a
        case Branch(left, right) => maximum(left).max(maximum(right))
      }
    }

    def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + depth(left).max(depth(right))
      }
    }

    def map[A,B](tree: Tree[A], f:A => B): Tree[B] = {
      tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left,f), map(right,f))
      }
    }

    def fold[A,B](tree: Tree[A], z:A => B, f:(B,B) => B): B = {
      tree match {
        case Leaf(a) => z(a)
        case Branch(left, right) => f(fold(left,z,f),fold(right,z,f))
      }
    }


    def size2[A](tree: Tree[A]): Int = {
      fold(tree, (a:A) => 1, (a:Int,b:Int) => 1 + a + b)
    }

    def maximum2(tree:Tree[Int]):Int = {
      fold(tree, (a:Int) => a, (a:Int,b:Int) => a.max(b))
    }

    def depth2(tree: Tree[Int]): Int = {
      fold(tree, (a:Int) => 1, (a:Int,b:Int) => 1 + a.max(b))
    }

    def map2[A,B](tree: Tree[A], f:A => B): Tree[B] = {
      fold(tree, (a:A) => Leaf(f(a)), (a:Tree[B],b:Tree[B]) => Branch(a,b))
    }


  }