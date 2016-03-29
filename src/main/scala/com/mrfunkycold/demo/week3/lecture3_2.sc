import com.mrfunkycold.demo.week2.Rational
//import com.mrfunkycold.demo.week2._
//import com.mrfunkycold.demo.week2.{Rational}

new Rational(1,2)

trait PrinterTest {
  val printMe = 0

  def mistake()
}

class MyClass extends PrinterTest {
  def mistake() = throw new Error("oops")
}

val c = new MyClass

c.printMe

c.mistake()
