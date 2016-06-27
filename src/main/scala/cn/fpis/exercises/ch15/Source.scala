package cn.fpis.exercises.ch15

import cn.fpis.exercises.ch13.IO

/**
 * Created by guillermo on 20/07/15.
 */
trait Source[O] {
  def |>[O2](p: Process[O,O2]): Source[O2]
  def filter(f: O => Boolean) = ??? //this |> Process.filter(f)
  def map[O2](f: O => O2) = ??? //this |> Process.lift(f)

}

case class ResourceR[R,I,O](acquire: IO[R], release: R => IO[Unit], step: R=>IO[Option[I]], trans: Process[I,O]) extends Source[O]{
  //This attach the process onto the output of the source
  def |>[O2](p: Process[O, O2]) = ResourceR(acquire, release, step, trans |> p)

  def collect: IO[IndexedSeq[O]] = {
    def go(acc: IndexedSeq[O], step: IO[Option[I]], p: Process[I,O], release: IO[Unit]): IndexedSeq[O] = ???

    acquire map (r => go(IndexedSeq(), step(r), trans, release(r)))

  }
}