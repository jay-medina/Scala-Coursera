val a = List(1,2,3,4,5,-1)

a.length //len of list

a.head //first item

a.tail //rest of the list without the head

a take 2 //takes the first n items

a drop 3 //drops first 3

a splitAt 3 // same as take and drop together

a(4) // same as a[4] in java

a.updated(1, 20) //returns a new list updated at n index

a.indexOf(-1) //indexOf an item

a.contains(2)

def last[T](xs: List[T]): T = xs match {
  case Nil => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}
a.last //last item
last(a)

def init[T](xs: List[T]): List[T] = xs match {
  case Nil => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y::init(ys)
}

a.init //list without the last item
init(a)

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys
  case c :: cs => c :: concat(cs, ys)
}

List(1,2,3) ++ List(9,8,7) //concats 2 list
concat(List(1,2,3), List(9,8,7))

def reverse[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case y :: ys => reverse(ys) ++ List(y)

}

a.reverse //reverse a list
reverse(a)

def removeAt[T](xs: List[T], n: Int): List[T] =
  (xs take n) ::: (xs drop n + 1)

removeAt(List(1,2,3,4,5), 2)
removeAt(List(1,2,3,4,5), -1)
removeAt(List(1,2,3,4,5), 0)

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (z::zs) :: ys => flatten(z::zs) ::: flatten(ys)
  case z :: zs => z :: flatten(zs)
}

flatten(List(List(1,1), 2, List(3, List(5,8))))




