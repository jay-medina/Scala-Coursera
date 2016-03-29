class Rational(x: Int, y: Int) {
  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val divisor = gcd(x,y)
  def numer = x / divisor
  def denom = y / divisor

  override def toString = numer + "/" + denom

  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom
    )

  def neg = new Rational(-numer, denom)
  def sub(that:Rational) = this.add(that.neg)

  def mul(that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)
}
val r = new Rational(1,2)
var r2 = new Rational(1,4)
var r3 = r.add(r2)
r3.sub(r2)
val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

x sub y sub z