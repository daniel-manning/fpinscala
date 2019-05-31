package chapter03

import org.scalatest.{FlatSpec, Matchers}

class FoldRightSpec extends FlatSpec with Matchers {
  "foldRight" should "reconstruct the list in cons" in {
    FoldRight.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldBe Cons(1,Cons(2,Cons(3,Nil)))
  }

  "foldRight" should "calculate the correct length of list" in {
    FoldRight.length(List(1,2,3)) shouldBe 3
  }

  "foldLeft" should "calculate the correct sum" in {
    FoldRight.foldLeft[Int,Int](List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30),0)(_+_) shouldBe 465
  }

  "sum2" should "total a list correctly" in {
    FoldRight.sum2(List(1,2,3,4,5)) shouldBe 15
  }

  "product2" should "total a list correctly" in {
    FoldRight.product2(List(1,2,3,4,5)) shouldBe 120
  }

  "reverse" should "reverse a list correctly" in {
    FoldRight.reverse(List(1,2,3,4,5)) shouldBe List(5,4,3,2,1)
  }

  "append" should "append a list correctly" in {
    FoldRight.append(List(1,2,3,4,5), List(6,7,8)) shouldBe List(1,2,3,4,5,6,7,8)
  }

  "flatten" should "flatten a list of lists" in {
    FoldRight.flatten(List(List(1),List(2,3),List(4,5,6))) shouldBe List(1,2,3,4,5,6)
  }

  "addOneToAll" should "add one to each member of a list" in {
    FoldRight.addOneToAll(List(1,2,3,4)) shouldBe List(2,3,4,5)
  }

  "convertElementsToString" should "change each member of a list to String" in {
    FoldRight.convertElementsToString(List(1.0,2.3,3.3334,4.221)) shouldBe List("1.0","2.3","3.3334","4.221")
  }

  "map" should "map over each member of a list" in {
    FoldRight.map(List(1,2,3,4))(_ + 1) shouldBe List(2,3,4,5)
  }

  "filter" should "remove elements of list that do not match a predicate" in {
    FoldRight.filter(List(1,2,3,4))(_ % 2 == 0) shouldBe List(2,4)
  }

  "flatMap" should "take a function making lists and flatten them" in {
    FoldRight.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "filter2" should "remove elements of list that do not match a predicate" in {
    FoldRight.filter2(List(1,2,3,4))(_ % 2 == 0) shouldBe List(2,4)
  }

  "plusTwoLists" should "add corresponding elements of two lists" in {
    FoldRight.plusTwoLists(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }

  "addTwoListsPairwise" should "add corresponding elements of two lists" in {
    FoldRight.addTwoListsPairwise(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }

  "zipWith" should "add corresponding elements of two lists" in {
    FoldRight.zipWith(List(1,2,3), List(4,5,6))((a:Int,b:Int)=> a*b) shouldBe List(4,10,18)
  }

  "hasSubsequence" should "find subsequence after a close match" in {
    FoldRight.hasSubsequence(List(1,2,5,1,2,3), List(1,2,3)) shouldBe true
  }

  "hasSubsequence" should "not find a subsequence for a near match" ignore {
    FoldRight.hasSubsequence(List(1,2,5,1), List(1,2,3)) shouldBe false
  }
}
