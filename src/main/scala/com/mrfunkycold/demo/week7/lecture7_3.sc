def exer() = {
  val x = {println('x'); 1}
  lazy val y = {println('y'); 2}
  def z = {println('z'); 3}

  x + y + z + x + y + z
}

exer()