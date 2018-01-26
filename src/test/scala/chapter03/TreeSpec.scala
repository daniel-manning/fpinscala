package chapter03

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "size" should "return the size of the tree" in {
    val leaf = Leaf(1)
    TreeMethods.size(leaf) shouldBe 1
  }

  "size" should "return the size of the treewith branches" in {
    val leaf = Branch(Leaf(2),Branch(Leaf(1),Leaf(3)))
    TreeMethods.size(leaf) shouldBe 5
  }

  "maximum" should "return the maximum of the tree" in {
    val tree = Branch(Leaf(2),Branch(Leaf(1),Leaf(3)))
    TreeMethods.maximum(tree) shouldBe 3
  }

  "depth" should "return the maximum of the tree of depth 1" in {
    val leaf = Leaf(1)
    TreeMethods.depth(leaf) shouldBe 1
  }

  "depth" should "return the maximum of the tree of depth 2" in {
    val branch = Branch(Leaf(1),Leaf(2))
    TreeMethods.depth(branch) shouldBe 2
  }

  "depth" should "return the maximum of the tree" in {
    val tree = Branch(Branch(Leaf(1),Leaf(4)),Branch(Branch(Branch(Leaf(2),Leaf(4)),Leaf(5)),Leaf(3)))
    TreeMethods.depth(tree) shouldBe 5
  }

  "map" should "return a tree with leaves modified by the function" in {
    val tree = Branch(Branch(Leaf(1),Leaf(4)),Branch(Branch(Branch(Leaf(2),Leaf(4)),Leaf(5)),Leaf(3)))
    val treeModified = Branch(Branch(Leaf(2),Leaf(5)),Branch(Branch(Branch(Leaf(3),Leaf(5)),Leaf(6)),Leaf(4)))
    TreeMethods.map(tree, (n: Int) => n + 1) shouldBe treeModified
  }

  "size2" should "return the size of the tree" in {
    val leaf = Leaf(1)
    TreeMethods.size2(leaf) shouldBe 1
  }

  "size2" should "return the size of the treewith branches" in {
    val leaf = Branch(Leaf(2),Branch(Leaf(1),Leaf(3)))
    TreeMethods.size2(leaf) shouldBe 5
  }

  "maximum2" should "return the maximum of the tree" in {
    val tree = Branch(Leaf(2),Branch(Leaf(1),Leaf(3)))
    TreeMethods.maximum2(tree) shouldBe 3
  }

  "depth2" should "return the maximum of the tree of depth 1" in {
    val leaf = Leaf(1)
    TreeMethods.depth2(leaf) shouldBe 1
  }

  "depth2" should "return the maximum of the tree of depth 2" in {
    val branch = Branch(Leaf(1),Leaf(2))
    TreeMethods.depth2(branch) shouldBe 2
  }

  "depth2" should "return the maximum of the tree" in {
    val tree = Branch(Branch(Leaf(1),Leaf(4)),Branch(Branch(Branch(Leaf(2),Leaf(4)),Leaf(5)),Leaf(3)))
    TreeMethods.depth2(tree) shouldBe 5
  }

  "map2" should "return a tree with leaves modified by the function" in {
    val tree = Branch(Branch(Leaf(1),Leaf(4)),Branch(Branch(Branch(Leaf(2),Leaf(4)),Leaf(5)),Leaf(3)))
    val treeModified = Branch(Branch(Leaf(2),Leaf(5)),Branch(Branch(Branch(Leaf(3),Leaf(5)),Leaf(6)),Leaf(4)))
    TreeMethods.map2(tree, (n: Int) => n + 1) shouldBe treeModified
  }

}
