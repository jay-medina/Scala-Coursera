package com.mrfunkycold.demo.week4

trait List2[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List2[T]
}

class Cons[T](val head: T, val tail: List2[T]) extends List2[T] {
  def isEmpty = false
}

class Nil[T] extends List2[T] {
  def head = throw new NoSuchElementException("Cannot call head on Nil")
  def tail = throw new NoSuchElementException("Cannot call tail on Nil")
  def isEmpty = true
}

object Nil {
  def apply = new Nil
}

object List2 {
  def apply[T]() = Nil
  def apply[T](x: T, y: T): List2[T] = new Cons(x, new Cons(y, new Nil))
  def apply[T](x: T): List2[T] = new Cons(x, new Nil)
}

object Test {
  def singleton[T](elem: T) = List2(elem)

  def nth[T](n: Int, li: List2[T]): T = {
    if(li.isEmpty) throw new IndexOutOfBoundsException
    else if(n == 0) li.head
    else nth(n-1, li.tail)
  }

  def main(args: Array[String]) {
    val temp = List2(4)

    val temp2 = List2(6, 8)
    val temp3 = List2(10, 12)

    println( nth(1, temp3) )
    //println(nth(-1, temp2))
  }
}