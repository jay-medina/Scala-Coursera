import scala.util.control.NonFatal

trait M[T] {
  def flatMap[U](f: T => U): M[U]
}

def unit[T](x:T): M[T]

abstract class Try[+T]
case class Success[T](x:T) extends Try[T]
case class Failure(ex: Exception) extends Try[Nothing]

object Try {

  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(ex) => Failure(ex)
    }
}