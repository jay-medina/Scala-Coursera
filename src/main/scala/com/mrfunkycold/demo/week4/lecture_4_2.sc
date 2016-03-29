abstract class Bool {
  def ifThenElse[T](t: T, e: T): T

  def && (x: => Bool): Bool = ifThenElse(x, new False)
  def || (x: => Bool): Bool = ifThenElse(new True, x)
}

class False extends Bool {
  def ifThenElse[T](t: T, e: T): T = e
}

class True extends Bool {
  def ifThenElse[T](t: T, e: T): T = t
}


abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor: Nat = throw new Error
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if(that.isZero) this else throw new Error

  override def toString = "0"
}

class Succ(n:Nat) extends Nat {
  def isZero = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if(that.isZero) this else n - that.predecessor

  override def toString = (Integer.valueOf(n.toString()) + 1).toString
}

val one = new Succ(Zero)
val two = new Succ(one)
val three = new Succ(two)

three - two
three + two
two - Zero