import cn.fpis.exercises.ch14.ST.runST
import cn.fpis.exercises.ch14.{ST, RunnableST, STRef}

for{
  r1 <- STRef[Nothing, Int](1)
  r2 <- STRef[Nothing, Int](2)
  v1 <- r1.read
  v2 <- r2.read
  _ <- r1.write(v1 + 5)
  _ <- r2.write(v2 + 7)
  v1 <- r1.read
  v2 <- r2.read
} yield(v1,v2)

val t = new RunnableST[(Int,Int)] {
  def apply[S] = for{
    r1 <- STRef(1)
    r2 <- STRef(2)
    v1 <- r1.read
    v2 <- r2.read
    _ <- r1.write(v1 + 5)
    _ <- r2.write(v2 + 7)
    v1 <- r1.read
    v2 <- r2.read
  } yield(v1,v2)
}

runST(t)
