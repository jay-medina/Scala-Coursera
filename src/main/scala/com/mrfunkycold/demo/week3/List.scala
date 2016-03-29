package com.mrfunkycold.demo.week3


trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def head = throw new NoSuchElementException("Cannot call head on Nil")
  def tail = throw new NoSuchElementException("Cannot call tail on Nil")
  def isEmpty = true
}

object Test {
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  def nth[T](n: Int, li: List[T]): T = {
    if(li.isEmpty) throw new IndexOutOfBoundsException
    else if(n == 0) li.head
    else nth(n-1, li.tail)
  }

  def main(args: Array[String]) {
    val temp = singleton(4)

    var temp2 = new Cons(6, temp)
    temp2 = new Cons(8, temp2)

    println( nth(1, temp2))
    println(nth(-1, temp2))
  }
}