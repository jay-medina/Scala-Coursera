def sum(a: Int, b: Int, f: Int => Int): Int =
  if(a > b) 0
  else f(a) + sum(a+1, b, f)

def sumTail(a:Int, b: Int, f: Int=>Int): Int = {
  def loop(a:Int, acc:Int): Int = {
    if(a > b) acc
    else loop(a+1, acc + f(a))
  }

  loop(a, 0)
}
def sumInts(a: Int, b: Int) = sumTail(a, b, x=> x)
def sumCubes(a: Int, b: Int) = sumTail(a, b, x => x*x*x)
def fact(a: Int): Int =
  if(a <= 1) 1
  else a * fact(a - 1)

def sumFactorials(a: Int, b: Int) = sumTail(a, b, fact)

(5 * 6) / 2

sumInts(1, 5)
