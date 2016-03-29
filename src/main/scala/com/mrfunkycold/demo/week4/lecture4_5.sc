trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  def isNumber: Boolean = true
  def isSum: Boolean = false
  def numValue: Int = n
  def leftOp: Expr = throw new Error
  def rightOp: Expr = throw new Error
}

class Sum(x1: Expr, x2: Expr) extends Expr {
  def isNumber: Boolean = false
  def isSum: Boolean = true
  def numValue: Int = throw new Error
  def leftOp: Expr = x1
  def rightOp: Expr = x2
}

def eval(e: Expr): Int = {
  if(e.isNumber) e.numValue
  else if(e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error
}

val s = new Sum(new Sum(new Number(1),new Number(2)),new Sum(new Number(5),new Number(10)))

eval(s)