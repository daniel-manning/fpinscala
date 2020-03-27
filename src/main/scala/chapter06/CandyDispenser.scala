package chapter06

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object World extends App {

  def performAction(input: Input)(machine: Machine): Machine = (input, machine) match {
    case (_, Machine(_, 0, _)) => {println("case1"); machine}
    case (Coin, Machine(true, candies, coins)) => {println("case2"); Machine(locked = false, candies, coins + 1)}
    case (Turn, Machine(true, candies, coins)) => {println("case3"); Machine(locked = true, candies - 1, coins)}
    case (Turn, Machine(false, _, _)) => {println("case4"); machine}
    case (Coin, Machine(false, _, _)) => {println("case5"); machine}
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft(State[Machine, Unit](machine => (machine, ()))){
      (machine, input) =>
        println(s"with input: $input and machine: $machine")
        machine.modify(performAction(input))
    }.get.map(m => (m.candies, m.coins))


  val machine = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run {
    Machine(locked = true, 10, 5)
  }

  println(s"We finish with a machine having: $machine")
}


