def radius = 10

def square(x: Double) = x * x

square(2)

square(5+4)

square(square(4))

def abs(x: Int) = if(x >= 0) x else -x

abs(-4)
abs(5)


def and(x: Boolean, y: => Boolean) = if(x) y else false
def or(x:Boolean, y: =>Boolean) = if(x) true else y


