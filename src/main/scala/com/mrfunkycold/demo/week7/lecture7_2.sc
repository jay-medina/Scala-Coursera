// Streams

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

// or

Stream(1,2,3)

(1 to 1000).toStream

def streamRange(lo: Int, hi: Int): Stream[Int] =
  if(lo >= hi) Stream.Empty
  else Stream.cons(lo, streamRange(lo + 1, hi))

def isPrime(n: Int): Boolean = {
  if(n <= 1 || n % 2 == 0) false
  else (2 to (n/2)) forall (x => n % x != 0)
}

((1000 to 10000).toStream filter isPrime)(1)