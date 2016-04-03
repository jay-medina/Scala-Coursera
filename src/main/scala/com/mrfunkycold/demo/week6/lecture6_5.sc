case class Book(title: String, authors: List[String]) {}

def mapFun[T, U](xs : List[T], f: T => U): List[U] =
  for(x <- xs) yield f(x)

def flatMap[T,U](xs: List[T], f: T=> Iterable[U]): List[U] =
  for(x <- xs; y<-f(x)) yield y

def filter[T](xs: List[T], p: T => Boolean): List[T] =
  for(x <- xs if p(x)) yield x


val books = Set(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "More Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

for(b <- books; a <- b.authors if a startsWith "Bird")
  yield b.title

books.flatMap(b =>
  //for (a <- b.authors if a startsWith "Bird") yield b.title
  //for (a <- b.authors withFilter (a => a startsWith "Bird")) yield b.title
  b.authors.withFilter(a=> a startsWith "Bird") map (y => b.title)
)

(List(1,2,3,4,5) foldLeft 0)((x, y) => x + y)