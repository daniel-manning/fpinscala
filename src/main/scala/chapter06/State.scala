package chapter06

case class State[S,+A](run: S => (A,S)){

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State( state => {
      val (a, s) = run(state)
      g(a).run(s)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))



}

object State {

  def unit[S, A](a: A): State[S, A] = State(state => (a, state))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty)){(a, b) => a.map2(b)(_ :: _)}

}
