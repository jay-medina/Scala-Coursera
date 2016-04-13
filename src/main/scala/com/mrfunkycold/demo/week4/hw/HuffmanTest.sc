

val chars = List('a','a','a','a','a','a','a','a',
  'b','b','b','c','d','e','f','g','h').map(_.toUpper)
//10001010
val cT = createCodeTree(chars); "AGBB"
val t2 = "1000101010".toList.map(_.asDigit)

decode(cT, t2)

decodedSecret.mkString
