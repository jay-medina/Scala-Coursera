val n = 7


def isPrime(n: Int): Boolean = {
  if(n <= 1 || n % 2 == 0) false
  else (2 to (n/2)) forall (x => n % x != 0)
}


val primePairs = (1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))
  ) filter {case (x,y)=> isPrime(x+y)}


//can be written as the following

val p2 = for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def oldscalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{case (x,y) => x * y}.sum

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (for((x,y) <- xs zip ys) yield x * y).sum


oldscalarProduct(Vector(1,2,3), Vector(4,5,6))
scalarProduct(Vector(1,2,3), Vector(4,5,6))