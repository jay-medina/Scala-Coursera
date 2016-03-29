def scaleList(xs: List[Double], factor: Double) =
  xs map (_ * factor)


scaleList(List(1,2,3,4), 2)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => List()
  case y :: ys => y * y :: squareList(ys)
}

def squareList2(xs: List[Int]) = xs map (x => x * x)

val li = List(4,3,2,1)

squareList(li)

squareList2(li)

li filter (x => x < 3)
li filterNot (x => x < 3)
li partition (x => x < 3)

li takeWhile (x => x > 2)
li dropWhile (x => x > 2)
li span (x => x > 2)

val test = List("a", "a", "a", "a", "b", "c", "c", "a")

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys => xs.takeWhile(_ == y) :: pack(xs.dropWhile(_ == y))
}

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map(x => (x.head, x.length))

pack(test)
encode(test)
