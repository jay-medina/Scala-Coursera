import scala.math.Ordering

val t1 = List(3,2,4,5,6,7,8,4,5,2,1,10)
val fruits = List("apple", "pineapple", "grapes", "oranges","banana", "kiwi")
val str = "Hello World"

def mSort[T](xs : List[T])(lt: (T,T) => Boolean): List[T] = {
  val n = xs.length / 2

  if( n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x1::xs1, y1::ys1) => if(lt(x1, y1)) x1 :: merge(xs1, ys)
      else y1 :: merge(xs, ys1)
      case _ => Nil

    }
    val (fst, snd) = xs.splitAt(n)
    merge(mSort(fst)(lt), mSort(snd)(lt))
  }
}

mSort(t1)((x,y) => x < y)
mSort(fruits)(_ < _)
mSort(str.toList)(_ < _)

def mSort2[T](xs : List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length / 2

  if( n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x1::xs1, y1::ys1) => if(ord.lt(x1, y1)) x1 :: merge(xs1, ys)
      else y1 :: merge(xs, ys1)
      case _ => Nil

    }
    val (fst, snd) = xs.splitAt(n)
    merge(mSort2(fst)(ord), mSort2(snd)(ord))
  }
}

mSort2(t1)(Ordering.Int)
mSort2(fruits)(Ordering.String)
mSort2("Hello World".toList)(Ordering.Char)


def impMSort[T](xs : List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2

  if( n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x1::xs1, y1::ys1) => if(ord.lt(x1, y1)) x1 :: merge(xs1, ys)
      else y1 :: merge(xs, ys1)
      case _ => Nil

    }
    val (fst, snd) = xs.splitAt(n)
    merge(impMSort(fst), impMSort(snd))
  }
}

impMSort(t1)
impMSort(fruits)
impMSort("Hello World".toList)