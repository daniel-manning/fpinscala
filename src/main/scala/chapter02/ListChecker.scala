package chapter02

object ListChecker {

     def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
          if (n >= as.length) -1
          else if (p(as(n))) n
          else loop(n + 1)
       loop(0)
      }

      def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
          @annotation.tailrec
          def loop(n: Int): Boolean =
            if(n >= as.length-1 ) true
            else if(ordered(as(n), as(n+1))) loop(n+1)
            else false
            loop(0)
     }

    }

object ListCheckerRunner extends App {
    val numArrayOutOfOrder = Array(1,2,4,3)
    val numArrayInOrder = Array(1,2,3,4)

    def ordered(a:Int,b:Int):Boolean = {
         a <= b
    }

    println(ListChecker.isSorted[Int](numArrayOutOfOrder, ordered))
    println(ListChecker.isSorted[Int](numArrayInOrder, ordered))
}