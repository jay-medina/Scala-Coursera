val t1 = List(1,2,3,4,5,6)

t1.reduce((x,y) => x + y)
t1.reduce((x,y) => x * y)
t1.product
t1.sum

t1 reduceLeft (_ + _)
t1 reduceLeft (_ * _)

t1.foldLeft(0)(_ + _)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_)::_)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, y) => y + 1)

mapFun(t1, Integer.toString)
lengthFun(t1)

