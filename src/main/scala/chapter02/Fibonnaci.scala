package chapter02

object Fibonacci {

      def fibonacci(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, acc: (Int,Int)): Int =
          if (n <= 0) acc._1
          else go(n - 1, (acc._2,acc._1 + acc._2))

          go(n, (0,1))
      }
}

object FibonacciRunner extends App {
    println(Fibonacci.fibonacci(8))
}
