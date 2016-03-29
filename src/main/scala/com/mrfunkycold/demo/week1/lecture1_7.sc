def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a%b)

def factorial(n: Int): Int = {
  def helper(s: Int, n: Int): Int =
    if (n == 1) s
    else helper(s * n, n - 1)
  helper(1, n)
}

gcd(10, 100)
gcd(100, 10)
gcd(24, 6)
gcd(225, 30)

factorial(5)
factorial(4)
