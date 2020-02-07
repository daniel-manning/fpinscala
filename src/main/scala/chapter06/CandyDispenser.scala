package chapter06

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

  /*object Nonsense {
   def update: Machine => ((Int, Int), Machine) = {
        machine =>

???
       }



    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      inputs.foldRight()

???
    }
}*/


