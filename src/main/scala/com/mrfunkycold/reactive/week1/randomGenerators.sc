import java.util.Random

trait Generator[+T] {
  self =>
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new Random
  def generate = rand.nextInt
}

val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}

val _booleans = for(x <- integers) yield x > 0

val pairs = new Generator[(Int, Int)] {
  def generate = (integers.generate, integers.generate)
}

def pairs[T,U](t: Generator[T], u: Generator[U]) = t flatMap {
  x => u map { y => (x, y)}
}

booleans.generate
pairs.generate

//*************** EXAMPLES ***************

def single[T](x : T): Generator[T] = new Generator[T] {
  override def generate: T = x
}

val s = single[String]("Hello")

for(x <- 1 until 3) yield s.generate

def choose(lo: Int, hi: Int): Generator[Int] =
  for(x <- integers) yield lo + x % (hi - lo)

val c = choose(4, 10)
for(x <- 1 to 3) yield c.generate

def oneOf[T](xs: T*): Generator[T] =
  for(idx <- choose(0, xs.length)) yield xs(idx)

oneOf(1,2,3,4,5).generate

def emptyLists = single(Nil)

def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if(isEmpty) emptyLists else nonEmptyLists
} yield list

//***************** EXERCISE *****************

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree

case class Leaf(x: Int) extends Tree

def leafs: Generator[Leaf] = for {
  x <- integers
} yield Leaf(x)

def inners: Generator[Inner] = for {
  x <- trees
  y <- trees
} yield Inner(x, y)

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if(isLeaf) leafs else inners
} yield tree


trees.generate










