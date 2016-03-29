abstract class IntSet {
  def incl(x: Int) : IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int) : IntSet =
    if( x < elem ) new NonEmpty(elem, left incl x, right)
    else if(x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def contains(x: Int): Boolean =
    if ( x < elem ) left contains x
    else if (x > elem) right contains x
    else true

  def union(other: IntSet): IntSet =
    left union right union other incl elem
  override def toString = "{" + left + elem + right + "}"
}

object Empty extends IntSet {
  def incl(x :Int) = new NonEmpty(x, Empty, Empty)
  def contains(x: Int) = false
  def union(other: IntSet): IntSet = other
  override def toString = "."
}
val t1 = new NonEmpty(4, Empty, Empty)
val t3 = t1.incl(3)
val t4 = new NonEmpty(10, Empty, Empty)
t3.contains(4)
t3.contains(3)
t3.contains(10)

t3 union t4 contains 10