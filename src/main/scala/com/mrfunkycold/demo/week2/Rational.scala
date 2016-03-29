package com.mrfunkycold.demo.week2

class Rational(x: Int, y: Int) {
  require(y > 0, "denominator cannot be zero")
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val divisor = Math.abs(gcd(x,y))
  val numer = x / divisor
  val denom = y / divisor


  override def toString = numer + "/" + denom

  def +(that: Rational) = {
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom
    )
  }

  def unary_- = new Rational(-numer, denom)
  def -(that:Rational) = this + -that

  def <(that: Rational) =
    (this.numer * that.denom) < (that.numer * this.denom)

  def max(that: Rational) =
    if (this < that) that else this

  def *(that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)
}

object Rational {
  def main(args: Array[String]) {

  }
}