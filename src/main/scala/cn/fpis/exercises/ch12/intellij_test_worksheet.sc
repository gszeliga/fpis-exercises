import cn.fpis.exercises.ch12.Traverse

Traverse.listTraverse.foldLeft(List("a","b"))("-hey!")((acc, v) => acc + v)
Traverse.listTraverse.foldLeft(List("a", "b", "c"))(0)((acc, s) => acc + 4)