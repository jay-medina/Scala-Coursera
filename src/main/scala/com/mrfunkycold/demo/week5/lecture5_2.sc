def mSort(xs : List[Int]): List[Int] = {
  val n = xs.length / 2

  if( n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x1::xs1, y1::ys1) => if(x1 < y1) x1 :: merge(xs1, ys)
      else y1 :: merge(xs, ys1)
      case _ => Nil

    }
    val (fst, snd) = xs.splitAt(n)
    merge(mSort(fst), mSort(snd))
  }
}

def mSort2(xs: List[Int]): List[Int] = {

  if(xs.length <= 1) xs
  else {
    val k = xs(0)
    val less = xs filter(_ < k)
    val greater = xs filter (_ >= k) drop 1

    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x1::xs1, y1::ys1) =>
        if(x1 < y1) x1 :: merge(xs1, ys)
        else y1 :: merge(xs, ys1)
      case _ => Nil

    }

    merge(mSort2(less), k :: mSort2(greater))
  }

}

val t1 = List(3,2,4,5,6,7,8,4,5,2,1,10)
val result = mSort(t1)
val result2 = mSort2(t1)
t1.length
result.length
result2.length