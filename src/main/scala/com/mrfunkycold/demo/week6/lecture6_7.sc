import scala.io.Source

val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

val words = in.getLines().toList.filter(word => word forall (_.isLetter))

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
)

/** Map from chars 'A-Z' to '0-9' */
val charCode: Map[Char, Char] =
  for{
    (digit, letters) <- mnem
    letter <- letters
  }
    yield letter -> digit
/** Maps a word to digit string */
def wordCode(word: String): String = word.toUpperCase.map(charCode)
wordCode("Java")

/**
  * "5282" -> List("Java", "Kata", "Lava", ...)
  * A Missing number should map to empty set
  * */
val wordsForNum: Map[String, Seq[String]] =
  words.groupBy(wordCode) withDefaultValue Seq()

wordsForNum("5282")


def encode(phoneNumber: String): Set[List[String]] =
  if(phoneNumber.isEmpty) Set(List())
  else {
    for {
      split <- 1 to phoneNumber.length
      word <- wordsForNum(phoneNumber take split)
      rest <- encode(phoneNumber drop split)
    }
      yield word :: rest
  }.toSet
encode("456835282")
def translate(number: String): Set[String] = encode(number).map(_.mkString(" "))

translate("7225247386")