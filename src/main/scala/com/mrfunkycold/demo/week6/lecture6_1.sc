val nums = Vector(1,2,3,-80)
val people = Vector("Bob", "James", "peter")

(20 +: nums).max
(nums :+ -20).min

val arr = Array(1,2,3,4)

arr map (x => x * 2)

val s = "Hello World"
s filter (x => x.isUpper)

//Ranges
val r = 1 until 5
val r1 = 1 to 5
1 to 10 by 3
6 to 1 by -2

//Functional methods
s exists (c => c.isLower)
s forall (c => c.isUpper)
arr.sum
arr.product

val pairs = List(1,2,3,4) zip s
pairs.unzip

s.flatMap(c => List('.',c))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ xy => xy match { case (x,y) => x * y }}.sum

def scalarProduct3(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{case (x,y) => x * y}.sum

def isPrime(n: Int) : Boolean = (2 until n) forall (x=> n % x != 0)

isPrime(10)

isPrime(11)
