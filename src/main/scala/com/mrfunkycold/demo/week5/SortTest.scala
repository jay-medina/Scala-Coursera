package com.mrfunkycold.demo.week5

/**
  * Created by jose on 3/21/16.
  */
object SortTest {

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

  def main(args: Array[String]) {

    val result2 = mSort2(List(3,2,4,5,6))

    println(result2)
  }

}
