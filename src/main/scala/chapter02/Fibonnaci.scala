package chapter02

object Fibonacci {

      def fibonacci(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, acc: (Int,Int)): Int =
          if (n <= 0) acc._1
          else go(n - 1, (acc._2,acc._1 + acc._2))

          go(n, (0,1))
      }

      def fibonacci(n: BigInt): BigInt = {
        @annotation.tailrec
        def go(n: BigInt, acc: (BigInt,BigInt)): BigInt =
          if (n <= 0) acc._1
          else go(n - 1, (acc._2,acc._1 + acc._2))

        go(n, (BigInt(0),BigInt(1)))
      }
}

object FibonacciRunner extends App {
    println(Fibonacci.fibonacci(BigInt(100)))
}
