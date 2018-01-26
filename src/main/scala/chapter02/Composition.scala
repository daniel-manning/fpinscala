package chapter02

object Composition {

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }

}

object CompositionRunner extends App {

  println ((Composition.compose(Math.sin,Math.cos)(0)) == Math.sin(Math.cos(0)))

}
