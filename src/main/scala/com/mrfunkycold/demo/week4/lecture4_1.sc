//The following two are the same
val f = (x: Int) => x * x


f(7)
f.apply(7)

val f2 = new Function1[Int, Int] {
  def apply(x: Int) = x * x
}

f2.apply(7)

f2(7)

