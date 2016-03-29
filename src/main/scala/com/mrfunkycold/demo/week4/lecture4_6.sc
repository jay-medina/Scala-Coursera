trait Expr {
  def eval : Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Sum(e1,e2) => "(" + e1.show + " + " + e2.show + ")"
    case Prod(e1,e2) => "(" + e1.show + " * " + e2.show + ")"
  }
}

case class Number(n: Int) extends Expr

case class Sum(x1: Expr, x2: Expr) extends Expr

case class Prod(x1: Expr, x2: Expr) extends Expr

val s = Prod(Sum(Number(1),Number(2)),Sum(Number(5),Number(10)))

s.show

s.eval