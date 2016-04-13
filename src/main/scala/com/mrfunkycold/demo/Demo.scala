
package com.mrfunkycold.demo

class Demo {

  def addTwoNumbers(a: Int, b: Int) = a + b
}

object Demo {

  def main(args: Array[String]) {
    println("Hello World")
    println(new Demo().addTwoNumbers(4,5))
  }
}
