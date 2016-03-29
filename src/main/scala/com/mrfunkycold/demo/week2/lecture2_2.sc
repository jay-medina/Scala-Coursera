/*def sum(f: Int=> Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int = {
    if(a > b) 0
    else f(a) + sumF(a+1, b)
  }
  sumF
} */
//This is the same as above
def reduce(f: Int => Int, combiner: (Int, Int) => Int, zero: Int)(a:Int, b: Int): Int = {
  if(a > b) zero
  else combiner(f(a), reduce(f, combiner, zero)(a+1, b) )
}
def sum(f: Int=> Int) = reduce(f, (x, y) => x+y, 0)_
def product(f: Int=> Int) = reduce(f, (x,y)=> x*y, 1)_
def fact(a: Int) = product(x=>x)(1, a)
def sumInts = sum(x=> x)
def sumCubes= sum(x => x * x * x)
def sumFactorials = sum(fact)
sumInts(1,5)
fact(4)
product(x => x * x)(3,4)