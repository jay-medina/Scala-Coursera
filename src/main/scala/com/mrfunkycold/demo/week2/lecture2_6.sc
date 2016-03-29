class Rational(x: Int, y: Int) {
  require(y > 0, "denominator cannot be zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val divisor = gcd(x,y)
  val numer = x / divisor
  val denom = y / divisor

  override def toString = numer + "/" + denom

  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom
    )

  def neg = new Rational(-numer, denom)
  def sub(that:Rational) = this.add(that.neg)

  def less(that: Rational) =
    (this.numer * that.denom) < (that.numer * this.denom)

  def max(that: Rational) =
    if (this.less(that)) that else this

  def mul(that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)
}
val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)
val a = new Rational(12)
y add y

y.max(z)

y.max(x)


