package cn.fpis.exercises.ch6

case class State[S, A](run: S => (A, S)) {

  def map[B](g: A => B): State[S, B] = {

    flatMap { v => State.unit(g(v)) }

  }

  def map2[B, C](g: State[S, B])(h: (A, B) => C): State[S, C] = {

    flatMap { a => g map { b => h(a, b) } }

  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {

    State(s =>
      {
        val (va, na) = run(s)
        g(va).run(na)
      })

  }

  def get[S]: State[S, S] = State(s => (s, s))

}

object State {

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {

    fs.foldRight(unit[S, List[A]](List.empty[A])) { (v, acc) =>
      v.map2(acc)(_ :: _)
    }

  }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}