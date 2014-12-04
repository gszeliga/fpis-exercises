import cn.fpis.exercises.ch13.IO

def double(v: Double)= v*2

def converter: IO[Unit] = {
  for {
    _ <- IO.printLine("Enter a temperature in degrees fahrenheit:")
    v <- IO.readLine2 map (_.toDouble)
    _ <- IO.printLine(double(v).toString)
  } yield ()
}

val t = IO.unit(2) ** IO.unit("AA")

t.run