val one = List(5,3,4,2,1)

val two = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

val t1 = List(7,3,9,2)


def insert(y: Int, ys: List[Int]) : List[Int] = ys match {
  case List() => List(y)
  case x :: xs => if(y < x) y :: ys
                  else x :: insert(y, xs)
}

def iSort(xs: List[Int]): List[Int] = xs match {
  case List() => Nil
  case y :: ys => insert(y, iSort(ys))
}

iSort(t1)

iSort(one)

val t3 = List(('a',2), ('b', 10))

t3.find((x) => x._1 == 'a')